// kotoba_runtime.cpp
// Session-based Kotoba runtime for "Kotoba-written apps"
// - Keeps global state across events
// - Reads one event per stdin line
// - Calls: 反応(入力, ボタン)  (ボタンは押されたボタンpayload。通常は空文字)
// - Expects Kotoba to return FULL UI JSON via builtins below
//
// Build (Termux):
//   clang++ -std=c++20 -O2 kotoba_runtime.cpp -o kotoba_runtime
//
// Run:
//   ./kotoba_runtime app.kb
// Then feed events:
//   TEXT:<user text>
//   BTN:<payload>
//
// Output: single-line JSON per event.
//
// Kotoba subset (same as previous engine):
//   つくる/もし/ほか/おわり/かえす/だす, 変数=式, + - * /, 比較
//
// UI DSL (builtins):
//   画面(text) -> base view object
//   入力(type, placeholder) -> input object
//   ボタン(label, payload) -> button object
//   遷移(state) -> next state hint
//   UI(view, input, btn1, btn2, ..., transit) -> final JSON string
//
// Recommended pattern in Kotoba:
//   かえす UI( 画面("..."), 入力("text","..."), ボタン("送る","send"), 遷移("talk") )

#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <unordered_map>
#include <map>
#include <memory>
#include <optional>
#include <stdexcept>
#include <iomanip>
#include <algorithm>
#include <utility>
#include <cctype>
using namespace std;

namespace kotoba {

static string json_escape(const string& s){
  string out; out.reserve(s.size()+8);
  for(unsigned char c: s){
    switch(c){
      case '\\': out += "\\\\"; break;
      case '"':  out += "\\\""; break;
      case '\n': out += "\\n";  break;
      case '\t': out += "\\t";  break;
      case '\r': out += "\\r";  break;
      default:
        if(c < 0x20){
          char buf[7];
          snprintf(buf, sizeof(buf), "\\u%04x", (int)c);
          out += buf;
        } else out.push_back((char)c);
    }
  }
  return out;
}

enum class TokKind {
  End, Newline,
  Ident, Number, String,
  LParen, RParen, Comma, Colon,
  Plus, Minus, Star, Slash,
  Assign,
  Eq, Ne, Lt, Le, Gt, Ge,
  KwMake, KwIf, KwElse, KwEnd, KwReturn, KwPrint
};

struct Token { TokKind kind; string text; int line,col; };

static inline bool is_space(char c){ return c==' '||c=='\t'||c=='\r'; }

struct Lexer {
  string src; size_t i=0; int line=1,col=1;
  explicit Lexer(string s): src(std::move(s)){}
  char peek() const { return i<src.size()?src[i]:'\0'; }
  char get(){
    char c=peek(); if(c=='\0') return c;
    i++;
    if(c=='\n'){ line++; col=1; } else col++;
    return c;
  }
  void skip_spaces(){
    while(true){
      char c=peek();
      if(c=='#'){ while(peek()!='\0' && peek()!='\n') get(); }
      else if(is_space(c)) get();
      else break;
    }
  }
  Token next(){
    skip_spaces();
    int tl=line, tc=col;
    char c=peek();
    if(c=='\0') return {TokKind::End,"",tl,tc};
    if(c=='\n'){ get(); return {TokKind::Newline,"\\n",tl,tc}; }

    if(c=='('){ get(); return {TokKind::LParen,"(",tl,tc}; }
    if(c==')'){ get(); return {TokKind::RParen,")",tl,tc}; }
    if(c==','){ get(); return {TokKind::Comma,",",tl,tc}; }
    if(c==':'){ get(); return {TokKind::Colon,":",tl,tc}; }
    if(c=='+'){ get(); return {TokKind::Plus,"+",tl,tc}; }
    if(c=='-'){ get(); return {TokKind::Minus,"-",tl,tc}; }
    if(c=='*'){ get(); return {TokKind::Star,"*",tl,tc}; }
    if(c=='/'){ get(); return {TokKind::Slash,"/",tl,tc}; }

    if(c=='='){
      get();
      if(peek()=='='){ get(); return {TokKind::Eq,"==",tl,tc}; }
      return {TokKind::Assign,"=",tl,tc};
    }
    if(c=='!'){
      get();
      if(peek()=='='){ get(); return {TokKind::Ne,"!=",tl,tc}; }
      throw runtime_error("Lexer error: unexpected '!'");
    }
    if(c=='<'){
      get();
      if(peek()=='='){ get(); return {TokKind::Le,"<=",tl,tc}; }
      return {TokKind::Lt,"<",tl,tc};
    }
    if(c=='>'){
      get();
      if(peek()=='='){ get(); return {TokKind::Ge,">=",tl,tc}; }
      return {TokKind::Gt,">",tl,tc};
    }

    if(c=='"'){
      get();
      string s;
      while(true){
        char d=get();
        if(d=='\0') throw runtime_error("Lexer error: unterminated string");
        if(d=='"') break;
        if(d=='\\'){
          char e=get();
          if(e=='n') s.push_back('\n');
          else if(e=='t') s.push_back('\t');
          else if(e=='r') s.push_back('\r');
          else if(e=='"') s.push_back('"');
          else if(e=='\\') s.push_back('\\');
          else s.push_back(e);
        } else s.push_back(d);
      }
      return {TokKind::String,s,tl,tc};
    }

    if((c>='0'&&c<='9') || (c=='.' && i+1<src.size() && isdigit((unsigned char)src[i+1]))){
      string n; bool dot=false;
      while(true){
        char d=peek();
        if(d=='.'){ if(dot) break; dot=true; n.push_back(get()); continue; }
        if(d>='0'&&d<='9'){ n.push_back(get()); continue; }
        break;
      }
      return {TokKind::Number,n,tl,tc};
    }

    auto is_stop=[&](char ch)->bool{
      if(ch=='\0'||ch=='\n') return true;
      if(is_space(ch)) return true;
      string stops="() ,:+-*/=<>!\"";
      return stops.find(ch)!=string::npos;
    };
    string id;
    while(!is_stop(peek())) id.push_back(get());

    if(id=="つくる") return {TokKind::KwMake,id,tl,tc};
    if(id=="もし")   return {TokKind::KwIf,id,tl,tc};
    if(id=="ほか")   return {TokKind::KwElse,id,tl,tc};
    if(id=="おわり") return {TokKind::KwEnd,id,tl,tc};
    if(id=="かえす") return {TokKind::KwReturn,id,tl,tc};
    if(id=="だす")   return {TokKind::KwPrint,id,tl,tc};

    return {TokKind::Ident,id,tl,tc};
  }
};

struct Expr; struct Stmt;
using ExprPtr=shared_ptr<Expr>;
using StmtPtr=shared_ptr<Stmt>;

enum class ValKind { Null, Number, String, Bool, Json }; // Json = raw JSON fragment/object

struct Value{
  ValKind kind=ValKind::Null;
  double num=0.0;
  string str;
  bool b=false;

  static Value Null(){ return {}; }
  static Value Number(double x){ Value v; v.kind=ValKind::Number; v.num=x; return v; }
  static Value String(string s){ Value v; v.kind=ValKind::String; v.str=std::move(s); return v; }
  static Value Bool(bool x){ Value v; v.kind=ValKind::Bool; v.b=x; return v; }
  static Value Json(string s){ Value v; v.kind=ValKind::Json; v.str=std::move(s); return v; }

  string to_string() const{
    switch(kind){
      case ValKind::Null: return "";
      case ValKind::Number:{
        ostringstream oss; oss.setf(std::ios::fixed); oss<<setprecision(12)<<num;
        string s=oss.str();
        while(s.size()>1 && s.find('.')!=string::npos && s.back()=='0') s.pop_back();
        if(!s.empty() && s.back()=='.') s.pop_back();
        return s;
      }
      case ValKind::String: return str;
      case ValKind::Bool: return b?"true":"false";
      case ValKind::Json: return str;
    }
    return "";
  }
  bool truthy() const{
    switch(kind){
      case ValKind::Null: return false;
      case ValKind::Bool: return b;
      case ValKind::Number: return num!=0.0;
      case ValKind::String: return !str.empty();
      case ValKind::Json: return !str.empty();
    }
    return false;
  }
};

enum class ExprKind { Literal, Var, Binary, Call };
struct Expr{
  ExprKind kind; int line=0,col=0;
  Value lit; string name;
  struct { string op; ExprPtr lhs,rhs; } bin;
  struct { string callee; vector<ExprPtr> args; } call;
  static ExprPtr literal(Value v,int l,int c){ auto e=make_shared<Expr>(); e->kind=ExprKind::Literal; e->lit=std::move(v); e->line=l;e->col=c; return e; }
  static ExprPtr var(string n,int l,int c){ auto e=make_shared<Expr>(); e->kind=ExprKind::Var; e->name=std::move(n); e->line=l;e->col=c; return e; }
  static ExprPtr binary(string op,ExprPtr a,ExprPtr b,int l,int c){ auto e=make_shared<Expr>(); e->kind=ExprKind::Binary; e->bin={std::move(op),std::move(a),std::move(b)}; e->line=l;e->col=c; return e; }
  static ExprPtr callf(string callee, vector<ExprPtr> args,int l,int c){ auto e=make_shared<Expr>(); e->kind=ExprKind::Call; e->call={std::move(callee),std::move(args)}; e->line=l;e->col=c; return e; }
};

enum class StmtKind { Assign, ExprOnly, If, Return, Print, FuncDef };
struct Stmt{
  StmtKind kind; int line=0,col=0;
  string var; ExprPtr expr;
  ExprPtr cond; vector<StmtPtr> thenBody, elseBody;
  string fname; vector<string> params; vector<StmtPtr> body;

  static StmtPtr assign(string v,ExprPtr e,int l,int c){ auto s=make_shared<Stmt>(); s->kind=StmtKind::Assign; s->var=std::move(v); s->expr=std::move(e); s->line=l;s->col=c; return s; }
  static StmtPtr exprOnly(ExprPtr e,int l,int c){ auto s=make_shared<Stmt>(); s->kind=StmtKind::ExprOnly; s->expr=std::move(e); s->line=l;s->col=c; return s; }
  static StmtPtr ifstmt(ExprPtr cnd,vector<StmtPtr> t,vector<StmtPtr> f,int l,int c){ auto s=make_shared<Stmt>(); s->kind=StmtKind::If; s->cond=std::move(cnd); s->thenBody=std::move(t); s->elseBody=std::move(f); s->line=l;s->col=c; return s; }
  static StmtPtr ret(ExprPtr e,int l,int c){ auto s=make_shared<Stmt>(); s->kind=StmtKind::Return; s->expr=std::move(e); s->line=l;s->col=c; return s; }
  static StmtPtr pr(ExprPtr e,int l,int c){ auto s=make_shared<Stmt>(); s->kind=StmtKind::Print; s->expr=std::move(e); s->line=l;s->col=c; return s; }
  static StmtPtr func(string n,vector<string> ps,vector<StmtPtr> b,int l,int c){ auto s=make_shared<Stmt>(); s->kind=StmtKind::FuncDef; s->fname=std::move(n); s->params=std::move(ps); s->body=std::move(b); s->line=l;s->col=c; return s; }
};

struct ParseError: runtime_error { using runtime_error::runtime_error; };

struct Parser{
  vector<Token> toks; size_t p=0;
  explicit Parser(vector<Token> t): toks(std::move(t)){}
  const Token& peek(int k=0) const{ size_t idx=min(p+(size_t)k,toks.size()-1); return toks[idx]; }
  bool match(TokKind k){ if(peek().kind==k){ p++; return true; } return false; }
  const Token& expect(TokKind k,const string& msg){
    if(peek().kind!=k){
      ostringstream oss; oss<<"Parse error ("<<peek().line<<":"<<peek().col<<"): "<<msg;
      throw ParseError(oss.str());
    }
    return toks[p++];
  }
  void eat_nl(){ while(match(TokKind::Newline)){} }

  int lbp(TokKind k){
    switch(k){
      case TokKind::Eq: case TokKind::Ne:
      case TokKind::Lt: case TokKind::Le: case TokKind::Gt: case TokKind::Ge: return 10;
      case TokKind::Plus: case TokKind::Minus: return 20;
      case TokKind::Star: case TokKind::Slash: return 30;
      default: return 0;
    }
  }

  ExprPtr primary(){
    const Token& t=peek();
    if(match(TokKind::Number)) return Expr::literal(Value::Number(stod(t.text)),t.line,t.col);
    if(match(TokKind::String)) return Expr::literal(Value::String(t.text),t.line,t.col);
    if(match(TokKind::Ident)){
      string name=t.text;
      if(match(TokKind::LParen)){
        vector<ExprPtr> args;
        if(!match(TokKind::RParen)){
          while(true){
            args.push_back(expr(0));
            if(match(TokKind::Comma)) continue;
            expect(TokKind::RParen,"`)` が必要です");
            break;
          }
        }
        return Expr::callf(name,std::move(args),t.line,t.col);
      }
      return Expr::var(name,t.line,t.col);
    }
    if(match(TokKind::LParen)){
      auto e=expr(0);
      expect(TokKind::RParen,"`)` が必要です");
      return e;
    }
    ostringstream oss; oss<<"Parse error ("<<t.line<<":"<<t.col<<"): 式が必要です";
    throw ParseError(oss.str());
  }

  ExprPtr expr(int minbp){
    auto left=primary();
    while(true){
      TokKind k=peek().kind;
      int bp=lbp(k);
      if(bp==0 || bp<minbp) break;
      Token opTok=peek(); p++;
      string op=opTok.text;
      auto right=expr(bp+1);
      left=Expr::binary(op,left,right,opTok.line,opTok.col);
    }
    return left;
  }

  vector<string> params(){
    vector<string> ps;
    expect(TokKind::LParen,"`(` が必要です");
    if(match(TokKind::RParen)) return ps;
    while(true){
      Token id=expect(TokKind::Ident,"引数名が必要です");
      ps.push_back(id.text);
      if(match(TokKind::Comma)) continue;
      expect(TokKind::RParen,"`)` が必要です");
      break;
    }
    return ps;
  }

  StmtPtr stmt(){
    eat_nl();
    const Token& t=peek();

    if(match(TokKind::KwMake)){
      Token name=expect(TokKind::Ident,"関数名が必要です");
      auto ps=params();
      expect(TokKind::Colon,"`:` が必要です");
      eat_nl();
      vector<StmtPtr> body;
      while(true){
        if(peek().kind==TokKind::KwEnd){ p++; break; }
        body.push_back(stmt());
      }
      return Stmt::func(name.text,std::move(ps),std::move(body),name.line,name.col);
    }

    if(match(TokKind::KwIf)){
      auto cond=expr(0);
      expect(TokKind::Colon,"`:` が必要です");
      eat_nl();
      vector<StmtPtr> thenB, elseB;

      while(true){
        if(peek().kind==TokKind::KwElse){
          p++; expect(TokKind::Colon,"`:` が必要です"); eat_nl();
          break;
        }
        if(peek().kind==TokKind::KwEnd){
          p++; return Stmt::ifstmt(cond,std::move(thenB),std::move(elseB),t.line,t.col);
        }
        thenB.push_back(stmt());
      }
      while(true){
        if(peek().kind==TokKind::KwEnd){ p++; break; }
        elseB.push_back(stmt());
      }
      return Stmt::ifstmt(cond,std::move(thenB),std::move(elseB),t.line,t.col);
    }

    if(match(TokKind::KwReturn)) return Stmt::ret(expr(0),t.line,t.col);
    if(match(TokKind::KwPrint))  return Stmt::pr(expr(0),t.line,t.col);

    if(peek().kind==TokKind::Ident && peek(1).kind==TokKind::Assign){
      Token id=peek(); p+=2;
      return Stmt::assign(id.text,expr(0),id.line,id.col);
    }

    return Stmt::exprOnly(expr(0),t.line,t.col);
  }

  vector<StmtPtr> program(){
    vector<StmtPtr> out;
    eat_nl();
    while(peek().kind!=TokKind::End){
      out.push_back(stmt());
      eat_nl();
    }
    return out;
  }
};

struct Function { vector<string> params; vector<StmtPtr> body; };
struct ReturnSignal { Value v; };

struct Env {
  unordered_map<string,Value> g;
  unordered_map<string,Function> f;
};

struct Eval {
  Env env;

  Value getv(const string& n){ auto it=env.g.find(n); return it==env.g.end()?Value::Null():it->second; }
  void setv(const string& n, Value v){ env.g[n]=std::move(v); }

  static double asnum(const Value& v){
    if(v.kind==ValKind::Number) return v.num;
    if(v.kind==ValKind::String){
      char* end=nullptr;
      double x=strtod(v.str.c_str(), &end);
      if(end && *end=='\0') return x;
    }
    return 0.0;
  }

  Value call_user(const Function& fn, const vector<Value>& args){
    vector<pair<string, optional<Value>>> saved;
    saved.reserve(fn.params.size());
    for(size_t i=0;i<fn.params.size();i++){
      const string& p=fn.params[i];
      auto it=env.g.find(p);
      if(it==env.g.end()) saved.push_back({p,nullopt});
      else saved.push_back({p,it->second});
      env.g[p]=(i<args.size()?args[i]:Value::Null());
    }
    try{
      exec_block(fn.body);
    }catch(ReturnSignal& rs){
      for(auto& s: saved){
        if(s.second.has_value()) env.g[s.first]=s.second.value();
        else env.g.erase(s.first);
      }
      return rs.v;
    }
    for(auto& s: saved){
      if(s.second.has_value()) env.g[s.first]=s.second.value();
      else env.g.erase(s.first);
    }
    return Value::Null();
  }

  Value eval_expr(const ExprPtr& e){
    switch(e->kind){
      case ExprKind::Literal: return e->lit;
      case ExprKind::Var: return getv(e->name);
      case ExprKind::Binary:{
        Value a=eval_expr(e->bin.lhs);
        Value b=eval_expr(e->bin.rhs);
        const string& op=e->bin.op;

        if(op=="+"){
          if(a.kind==ValKind::String || b.kind==ValKind::String) return Value::String(a.to_string()+b.to_string());
          return Value::Number(asnum(a)+asnum(b));
        }
        if(op=="-") return Value::Number(asnum(a)-asnum(b));
        if(op=="*") return Value::Number(asnum(a)*asnum(b));
        if(op=="/") return Value::Number(asnum(a)/asnum(b));

        string as=a.to_string(), bs=b.to_string();
        if(op=="==") return Value::Bool(as==bs);
        if(op=="!=") return Value::Bool(as!=bs);
        if(op=="<")  return Value::Bool(as<bs);
        if(op=="<=") return Value::Bool(as<=bs);
        if(op==">")  return Value::Bool(as>bs);
        if(op==">=") return Value::Bool(as>=bs);
        return Value::Null();
      }
      case ExprKind::Call:{
        const string& cal=e->call.callee;

        // --- UI DSL builtins (return Json fragments) ---
        if(cal=="画面"){
          string text = e->call.args.size()>=1 ? eval_expr(e->call.args[0]).to_string() : "";
          string j = string("{\"text\":\"")+json_escape(text)+"\"}";
          return Value::Json(j);
        }
        if(cal=="入力"){
          string type = e->call.args.size()>=1 ? eval_expr(e->call.args[0]).to_string() : "text";
          string ph   = e->call.args.size()>=2 ? eval_expr(e->call.args[1]).to_string() : "";
          // type: "text" | "none"
          string j = string("{\"type\":\"")+json_escape(type)+"\",\"placeholder\":\""+json_escape(ph)+"\"}";
          return Value::Json(j);
        }
        if(cal=="ボタン"){
          string label = e->call.args.size()>=1 ? eval_expr(e->call.args[0]).to_string() : "";
          string payload= e->call.args.size()>=2 ? eval_expr(e->call.args[1]).to_string() : label;
          string j = string("{\"label\":\"")+json_escape(label)+"\",\"payload\":\""+json_escape(payload)+"\"}";
          return Value::Json(j);
        }
        if(cal=="遷移"){
          string st = e->call.args.size()>=1 ? eval_expr(e->call.args[0]).to_string() : "";
          string j = string("{\"next\":\"")+json_escape(st)+"\"}";
          return Value::Json(j);
        }
        if(cal=="UI"){
          // UI(view, input, btn..., transit)
          // Any arg can be Json or String; Strings treated as view text fallback.
          string text="";
          string inputObj="null";
          vector<string> btns;
          string next="";

          for(auto& a: e->call.args){
            Value v = eval_expr(a);
            if(v.kind==ValKind::Json){
              // detect which object by keys
              if(v.str.find("\"text\"")!=string::npos) {
                // extract text (cheap)
                auto p=v.str.find("\"text\":\""); if(p!=string::npos){ p+=8; auto q=v.str.find("\"",p); text=v.str.substr(p,q-p); }
              } else if(v.str.find("\"type\"")!=string::npos) {
                inputObj=v.str;
              } else if(v.str.find("\"label\"")!=string::npos) {
                btns.push_back(v.str);
              } else if(v.str.find("\"next\"")!=string::npos) {
                auto p=v.str.find("\"next\":\""); if(p!=string::npos){ p+=8; auto q=v.str.find("\"",p); next=v.str.substr(p,q-p); }
              }
            } else {
              // plain string -> text
              if(text.empty()) text = json_escape(v.to_string());
            }
          }

          string out="{";
          out += "\"text\":\""+text+"\",";
          out += "\"input\":"+(inputObj=="null"?string("null"):inputObj)+",";
          out += "\"buttons\":[";
          for(size_t i=0;i<btns.size();i++){
            if(i) out += ",";
            out += btns[i];
          }
          out += "]";
          if(!next.empty()) out += ",\"next\":\""+json_escape(next)+"\"";
          out += "}";
          return Value::String(out); // final JSON
        }

        // --- user-defined functions ---
        auto it=env.f.find(cal);
        if(it==env.f.end()) return Value::Null();
        vector<Value> args;
        args.reserve(e->call.args.size());
        for(auto& a: e->call.args) args.push_back(eval_expr(a));
        return call_user(it->second, args);
      }
    }
    return Value::Null();
  }

  void exec_stmt(const StmtPtr& s){
    switch(s->kind){
      case StmtKind::Assign: setv(s->var, eval_expr(s->expr)); return;
      case StmtKind::ExprOnly: (void)eval_expr(s->expr); return;
      case StmtKind::Print: cout<<eval_expr(s->expr).to_string()<<"\n"; return;
      case StmtKind::Return: throw ReturnSignal{eval_expr(s->expr)};
      case StmtKind::If:
        if(eval_expr(s->cond).truthy()) exec_block(s->thenBody);
        else exec_block(s->elseBody);
        return;
      case StmtKind::FuncDef:
        env.f[s->fname]=Function{s->params,s->body};
        return;
    }
  }

  void exec_block(const vector<StmtPtr>& b){ for(auto& st:b) exec_stmt(st); }

  void load(const vector<StmtPtr>& prog){
    for(auto& st: prog) if(st->kind==StmtKind::FuncDef) exec
