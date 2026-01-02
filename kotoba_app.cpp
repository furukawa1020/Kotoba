// kotoba_app.cpp
// Session-based Kotoba engine for app runtime (stdin event loop -> stdout JSON view)
//
// Build (Termux):
//   clang++ -std=c++20 -O2 kotoba_app.cpp -o kotoba_app
//
// Run:
//   ./kotoba_app app.kb
// Then type lines (events) into stdin. Each line becomes 入力.
// Engine calls 反応(入力) and prints returned JSON (single line).
//
// Kotoba language (subset, Japanese keywords):
//   - function: つくる 名前(a,b): ... おわり
//   - if: もし 条件: ... ほか: ... おわり
//   - return: かえす 式
//   - print: だす 式
//   - assignment: 変数 = 式
//   - expressions: + - * /, == != < <= > >=, parentheses, string "..."
//
// Builtin:
//   画面(text, button1, button2, ...) -> JSON string: {"text":"...","buttons":[...]}
//
// App contract:
//   Define function: つくる 反応(入力): ... かえす 画面("...", "...") おわり
//   Global vars persist across events (session state).

#include <bits/stdc++.h>
using namespace std;

namespace kotoba {

// ---------- Utilities ----------
static inline bool is_space(char c){ return c==' '||c=='\t'||c=='\r'; }

static string json_escape(const string& s){
  string out;
  out.reserve(s.size()+8);
  for(unsigned char c: s){
    switch(c){
      case '\\': out += "\\\\"; break;
      case '"':  out += "\\\""; break;
      case '\n': out += "\\n";  break;
      case '\t': out += "\\t";  break;
      case '\r': out += "\\r";  break;
      default:
        if (c < 0x20) {
          // control chars -> \u00XX
          char buf[7];
          snprintf(buf, sizeof(buf), "\\u%04x", (int)c);
          out += buf;
        } else out.push_back((char)c);
    }
  }
  return out;
}

// ---------- Tokenizer ----------
enum class TokKind {
  End, Newline,
  Ident, Number, String,
  LParen, RParen, Comma, Colon,
  Plus, Minus, Star, Slash,
  Assign,
  Eq, Ne, Lt, Le, Gt, Ge,
  KwMake, KwIf, KwElse, KwEnd, KwReturn, KwPrint
};

struct Token {
  TokKind kind;
  string text;
  int line, col;
};

struct Lexer {
  string src;
  size_t i=0;
  int line=1, col=1;

  explicit Lexer(string s): src(std::move(s)) {}

  char peek() const { return i < src.size() ? src[i] : '\0'; }
  char get(){
    char c = peek();
    if(c=='\0') return c;
    i++;
    if(c=='\n'){ line++; col=1; }
    else col++;
    return c;
  }

  void skip_spaces(){
    while(true){
      char c = peek();
      if(c=='#'){ // comment to endline
        while(peek()!='\0' && peek()!='\n') get();
      } else if(is_space(c)) get();
      else break;
    }
  }

  Token next(){
    skip_spaces();
    int tl=line, tc=col;
    char c = peek();
    if(c=='\0') return {TokKind::End,"",tl,tc};
    if(c=='\n'){ get(); return {TokKind::Newline,"\\n",tl,tc}; }

    // punct
    if(c=='('){ get(); return {TokKind::LParen,"(",tl,tc}; }
    if(c==')'){ get(); return {TokKind::RParen,")",tl,tc}; }
    if(c==','){ get(); return {TokKind::Comma,",",tl,tc}; }
    if(c==':'){ get(); return {TokKind::Colon,":",tl,tc}; }
    if(c=='+'){ get(); return {TokKind::Plus,"+",tl,tc}; }
    if(c=='-'){ get(); return {TokKind::Minus,"-",tl,tc}; }
    if(c=='*'){ get(); return {TokKind::Star,"*",tl,tc}; }
    if(c=='/'){ get(); return {TokKind::Slash,"/",tl,tc}; }

    // operators
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

    // string
    if(c=='"'){
      get(); // "
      string s;
      while(true){
        char d = get();
        if(d=='\0') throw runtime_error("Lexer error: unterminated string");
        if(d=='"') break;
        if(d=='\\'){
          char e = get();
          if(e=='n') s.push_back('\n');
          else if(e=='t') s.push_back('\t');
          else if(e=='r') s.push_back('\r');
          else if(e=='"') s.push_back('"');
          else if(e=='\\') s.push_back('\\');
          else s.push_back(e);
        } else s.push_back(d);
      }
      return {TokKind::String, s, tl, tc};
    }

    // number
    if((c>='0'&&c<='9') || (c=='.' && isdigit((unsigned char)src[i+1]))){
      string n;
      bool dot=false;
      while(true){
        char d = peek();
        if(d=='.'){ if(dot) break; dot=true; n.push_back(get()); continue; }
        if(d>='0'&&d<='9'){ n.push_back(get()); continue; }
        break;
      }
      return {TokKind::Number, n, tl, tc};
    }

    // ident / keywords (allow UTF-8 bytes; treat as non-space non-punct sequence)
    // We'll stop at space, newline, or known punct/operators.
    auto is_stop = [&](char ch)->bool{
      if(ch=='\0'||ch=='\n') return true;
      if(is_space(ch)) return true;
      string stops="() ,:+-*/=<>!\"";
      return stops.find(ch)!=string::npos;
    };

    string id;
    while(!is_stop(peek())){
      id.push_back(get());
    }

    // keywords (Japanese)
    if(id=="つくる") return {TokKind::KwMake,id,tl,tc};
    if(id=="もし")   return {TokKind::KwIf,id,tl,tc};
    if(id=="ほか")   return {TokKind::KwElse,id,tl,tc};
    if(id=="おわり") return {TokKind::KwEnd,id,tl,tc};
    if(id=="かえす") return {TokKind::KwReturn,id,tl,tc};
    if(id=="だす")   return {TokKind::KwPrint,id,tl,tc};

    return {TokKind::Ident,id,tl,tc};
  }
};

// ---------- AST ----------
struct Expr;
struct Stmt;

using ExprPtr = shared_ptr<Expr>;
using StmtPtr = shared_ptr<Stmt>;

enum class ValKind { Null, Number, String, Bool };

struct Value {
  ValKind kind = ValKind::Null;
  double num = 0.0;
  string str;
  bool b = false;

  static Value Null(){ return {}; }
  static Value Number(double x){ Value v; v.kind=ValKind::Number; v.num=x; return v; }
  static Value String(string s){ Value v; v.kind=ValKind::String; v.str=std::move(s); return v; }
  static Value Bool(bool x){ Value v; v.kind=ValKind::Bool; v.b=x; return v; }

  string to_string() const {
    switch(kind){
      case ValKind::Null: return "";
      case ValKind::Number:{
        // avoid trailing zeros
        std::ostringstream oss;
        oss.setf(std::ios::fixed); oss<<setprecision(12)<<num;
        string s=oss.str();
        while(s.size()>1 && s.find('.')!=string::npos && s.back()=='0') s.pop_back();
        if(!s.empty() && s.back()=='.') s.pop_back();
        return s;
      }
      case ValKind::String: return str;
      case ValKind::Bool: return b ? "true" : "false";
    }
    return "";
  }

  bool truthy() const {
    switch(kind){
      case ValKind::Null: return false;
      case ValKind::Bool: return b;
      case ValKind::Number: return num != 0.0;
      case ValKind::String: return !str.empty();
    }
    return false;
  }
};

enum class ExprKind { Literal, Var, Binary, Call };

struct Expr {
  ExprKind kind;
  int line=0,col=0;
  // variants
  Value lit;
  string name;
  struct { string op; ExprPtr lhs, rhs; } bin;
  struct { string callee; vector<ExprPtr> args; } call;

  static ExprPtr literal(Value v,int l,int c){
    auto e=make_shared<Expr>(); e->kind=ExprKind::Literal; e->lit=std::move(v); e->line=l;e->col=c; return e;
  }
  static ExprPtr var(string n,int l,int c){
    auto e=make_shared<Expr>(); e->kind=ExprKind::Var; e->name=std::move(n); e->line=l;e->col=c; return e;
  }
  static ExprPtr binary(string op,ExprPtr a,ExprPtr b,int l,int c){
    auto e=make_shared<Expr>(); e->kind=ExprKind::Binary; e->bin={std::move(op),std::move(a),std::move(b)}; e->line=l;e->col=c; return e;
  }
  static ExprPtr callf(string callee, vector<ExprPtr> args,int l,int c){
    auto e=make_shared<Expr>(); e->kind=ExprKind::Call; e->call={std::move(callee),std::move(args)}; e->line=l;e->col=c; return e;
  }
};

enum class StmtKind { Assign, ExprOnly, If, Return, Print, FuncDef };

struct Stmt {
  StmtKind kind;
  int line=0,col=0;

  // Assign
  string var;
  ExprPtr expr;

  // If
  ExprPtr cond;
  vector<StmtPtr> thenBody;
  vector<StmtPtr> elseBody;

  // Return / Print use expr

  // FuncDef
  string fname;
  vector<string> params;
  vector<StmtPtr> body;

  static StmtPtr assign(string v, ExprPtr e,int l,int c){
    auto s=make_shared<Stmt>(); s->kind=StmtKind::Assign; s->var=std::move(v); s->expr=std::move(e); s->line=l;s->col=c; return s;
  }
  static StmtPtr exprOnly(ExprPtr e,int l,int c){
    auto s=make_shared<Stmt>(); s->kind=StmtKind::ExprOnly; s->expr=std::move(e); s->line=l;s->col=c; return s;
  }
  static StmtPtr ifstmt(ExprPtr cond, vector<StmtPtr> t, vector<StmtPtr> f,int l,int c){
    auto s=make_shared<Stmt>(); s->kind=StmtKind::If; s->cond=std::move(cond); s->thenBody=std::move(t); s->elseBody=std::move(f); s->line=l;s->col=c; return s;
  }
  static StmtPtr ret(ExprPtr e,int l,int c){
    auto s=make_shared<Stmt>(); s->kind=StmtKind::Return; s->expr=std::move(e); s->line=l;s->col=c; return s;
  }
  static StmtPtr pr(ExprPtr e,int l,int c){
    auto s=make_shared<Stmt>(); s->kind=StmtKind::Print; s->expr=std::move(e); s->line=l;s->col=c; return s;
  }
  static StmtPtr func(string name, vector<string> params, vector<StmtPtr> body,int l,int c){
    auto s=make_shared<Stmt>(); s->kind=StmtKind::FuncDef; s->fname=std::move(name); s->params=std::move(params); s->body=std::move(body); s->line=l;s->col=c; return s;
  }
};

// ---------- Parser ----------
struct ParseError : std::runtime_error {
  using std::runtime_error::runtime_error;
};

struct Parser {
  vector<Token> toks;
  size_t p=0;

  explicit Parser(vector<Token> t): toks(std::move(t)) {}

  const Token& peek(int k=0) const {
    size_t idx = min(p+(size_t)k, toks.size()-1);
    return toks[idx];
  }
  bool match(TokKind k){
    if(peek().kind==k){ p++; return true; }
    return false;
  }
  const Token& expect(TokKind k, const string& msg){
    if(peek().kind!=k){
      ostringstream oss;
      oss<<"Parse error ("<<peek().line<<":"<<peek().col<<"): "<<msg;
      throw ParseError(oss.str());
    }
    return toks[p++];
  }
  void eat_newlines(){
    while(match(TokKind::Newline)){}
  }

  int lbp(TokKind k){
    switch(k){
      case TokKind::Eq: case TokKind::Ne:
      case TokKind::Lt: case TokKind::Le: case TokKind::Gt: case TokKind::Ge:
        return 10;
      case TokKind::Plus: case TokKind::Minus: return 20;
      case TokKind::Star: case TokKind::Slash: return 30;
      default: return 0;
    }
  }

  ExprPtr parse_primary(){
    const Token& t = peek();
    if(match(TokKind::Number)){
      return Expr::literal(Value::Number(std::stod(t.text)), t.line,t.col);
    }
    if(match(TokKind::String)){
      return Expr::literal(Value::String(t.text), t.line,t.col);
    }
    if(match(TokKind::Ident)){
      // call or var
      string name=t.text;
      if(match(TokKind::LParen)){
        vector<ExprPtr> args;
        if(!match(TokKind::RParen)){
          while(true){
            args.push_back(parse_expr(0));
            if(match(TokKind::Comma)) continue;
            expect(TokKind::RParen, "`)` が必要です");
            break;
          }
        }
        return Expr::callf(name, std::move(args), t.line,t.col);
      }
      return Expr::var(name, t.line,t.col);
    }
    if(match(TokKind::LParen)){
      auto e = parse_expr(0);
      expect(TokKind::RParen, "`)` が必要です");
      return e;
    }
    ostringstream oss;
    oss<<"Parse error ("<<t.line<<":"<<t.col<<"): 式が必要です";
    throw ParseError(oss.str());
  }

  ExprPtr parse_expr(int minbp){
    auto left = parse_primary();
    while(true){
      TokKind k = peek().kind;
      int bp = lbp(k);
      if(bp==0 || bp < minbp) break;

      Token opTok = peek();
      p++;
      string op = opTok.text;

      int rbp = bp+1;
      auto right = parse_expr(rbp);
      left = Expr::binary(op, left, right, opTok.line, opTok.col);
    }
    return left;
  }

  vector<string> parse_param_list(){
    vector<string> ps;
    expect(TokKind::LParen, "`(` が必要です");
    if(match(TokKind::RParen)) return ps;
    while(true){
      Token id = expect(TokKind::Ident, "引数名が必要です");
      ps.push_back(id.text);
      if(match(TokKind::Comma)) continue;
      expect(TokKind::RParen, "`)` が必要です");
      break;
    }
    return ps;
  }

  StmtPtr parse_stmt(){
    eat_newlines();
    const Token& t = peek();

    // function def
    if(match(TokKind::KwMake)){
      Token name = expect(TokKind::Ident, "関数名が必要です");
      auto params = parse_param_list();
      expect(TokKind::Colon, "`:` が必要です");
      eat_newlines();

      vector<StmtPtr> body;
      while(true){
        if(peek().kind==TokKind::KwEnd){
          p++; // consume おわり
          break;
        }
        body.push_back(parse_stmt());
      }
      return Stmt::func(name.text, std::move(params), std::move(body), name.line,name.col);
    }

    // if
    if(match(TokKind::KwIf)){
      auto cond = parse_expr(0);
      expect(TokKind::Colon, "`:` が必要です");
      eat_newlines();

      vector<StmtPtr> thenBody;
      vector<StmtPtr> elseBody;

      while(true){
        if(peek().kind==TokKind::KwElse){
          p++; // ほか
          expect(TokKind::Colon, "`:` が必要です");
          eat_newlines();
          break;
        }
        if(peek().kind==TokKind::KwEnd){
          p++; // おわり
          return Stmt::ifstmt(cond, std::move(thenBody), std::move(elseBody), t.line,t.col);
        }
        thenBody.push_back(parse_stmt());
      }

      while(true){
        if(peek().kind==TokKind::KwEnd){
          p++;
          break;
        }
        elseBody.push_back(parse_stmt());
      }

      return Stmt::ifstmt(cond, std::move(thenBody), std::move(elseBody), t.line,t.col);
    }

    // return
    if(match(TokKind::KwReturn)){
      auto e = parse_expr(0);
      return Stmt::ret(e, t.line,t.col);
    }

    // print
    if(match(TokKind::KwPrint)){
      auto e = parse_expr(0);
      return Stmt::pr(e, t.line,t.col);
    }

    // assignment or expr
    if(peek().kind==TokKind::Ident && peek(1).kind==TokKind::Assign){
      Token id = peek();
      p+=2; // ident =
      auto e = parse_expr(0);
      return Stmt::assign(id.text, e, id.line,id.col);
    }

    // expression statement
    auto e = parse_expr(0);
    return Stmt::exprOnly(e, t.line,t.col);
  }

  vector<StmtPtr> parse_program(){
    vector<StmtPtr> out;
    eat_newlines();
    while(peek().kind!=TokKind::End){
      out.push_back(parse_stmt());
      eat_newlines();
    }
    return out;
  }
};

// ---------- Runtime ----------
struct Function {
  vector<string> params;
  vector<StmtPtr> body;
};

struct ReturnSignal {
  Value value;
};

struct Env {
  // session globals
  unordered_map<string, Value> globals;
  unordered_map<string, Function> funcs;
};

struct Evaluator {
  Env env;

  Value get_var(const string& name){
    auto it = env.globals.find(name);
    if(it!=env.globals.end()) return it->second;
    return Value::Null();
  }
  void set_var(const string& name, Value v){
    env.globals[name]=std::move(v);
  }

  Value eval_expr(const ExprPtr& e){
    switch(e->kind){
      case ExprKind::Literal: return e->lit;
      case ExprKind::Var: return get_var(e->name);
      case ExprKind::Binary: {
        Value a = eval_expr(e->bin.lhs);
        Value b = eval_expr(e->bin.rhs);
        const string& op = e->bin.op;

        auto num = [&](const Value& v)->double{
          if(v.kind==ValKind::Number) return v.num;
          // try parse numeric string
          if(v.kind==ValKind::String){
            char* end=nullptr;
            double x=strtod(v.str.c_str(), &end);
            if(end && *end=='\0') return x;
          }
          return 0.0;
        };

        if(op=="+"){
          // string concat if either is string
          if(a.kind==ValKind::String || b.kind==ValKind::String){
            return Value::String(a.to_string() + b.to_string());
          }
          return Value::Number(num(a)+num(b));
        }
        if(op=="-") return Value::Number(num(a)-num(b));
        if(op=="*") return Value::Number(num(a)*num(b));
        if(op=="/") return Value::Number(num(a)/num(b));

        auto cmp_str = [&](const Value& x, const Value& y){
          return x.to_string().compare(y.to_string());
        };

        if(op=="==") return Value::Bool(a.to_string()==b.to_string());
        if(op=="!=") return Value::Bool(a.to_string()!=b.to_string());

        if(op=="<")  return Value::Bool(cmp_str(a,b)<0);
        if(op=="<=") return Value::Bool(cmp_str(a,b)<=0);
        if(op==">")  return Value::Bool(cmp_str(a,b)>0);
        if(op==">=") return Value::Bool(cmp_str(a,b)>=0);

        return Value::Null();
      }
      case ExprKind::Call: {
        // builtin: 画面(...)
        if(e->call.callee=="画面"){
          vector<Value> vs;
          vs.reserve(e->call.args.size());
          for(auto& a: e->call.args) vs.push_back(eval_expr(a));
          string text = vs.size()>=1 ? vs[0].to_string() : "";
          vector<string> buttons;
          for(size_t i=1;i<vs.size();i++) buttons.push_back(vs[i].to_string());

          string json = "{\"text\":\""+json_escape(text)+"\",\"buttons\":[";
          for(size_t i=0;i<buttons.size();i++){
            if(i) json += ",";
            json += "\""+json_escape(buttons[i])+"\"";
          }
          json += "]}";
          return Value::String(json);
        }

        // user-defined
        auto it = env.funcs.find(e->call.callee);
        if(it==env.funcs.end()){
          // unknown function -> null
          return Value::Null();
        }

        // evaluate args
        vector<Value> args;
        args.reserve(e->call.args.size());
        for(auto& a: e->call.args) args.push_back(eval_expr(a));

        return call_user(it->second, args);
      }
    }
    return Value::Null();
  }

  Value call_user(const Function& fn, const vector<Value>& args){
    // create local scope overlay on globals (simple: store & restore modified globals for params)
    vector<pair<string, optional<Value>>> saved;
    saved.reserve(fn.params.size());
    for(size_t i=0;i<fn.params.size();i++){
      const string& p = fn.params[i];
      auto it = env.globals.find(p);
      if(it==env.globals.end()) saved.push_back({p, nullopt});
      else saved.push_back({p, it->second});
      env.globals[p] = (i<args.size()) ? args[i] : Value::Null();
    }

    try{
      exec_block(fn.body);
    } catch (ReturnSignal& rs){
      // restore params
      for(auto& s: saved){
        if(s.second.has_value()) env.globals[s.first]=s.second.value();
        else env.globals.erase(s.first);
      }
      return rs.value;
    }

    // restore params
    for(auto& s: saved){
      if(s.second.has_value()) env.globals[s.first]=s.second.value();
      else env.globals.erase(s.first);
    }
    return Value::Null();
  }

  void exec_stmt(const StmtPtr& s){
    switch(s->kind){
      case StmtKind::Assign: {
        Value v = eval_expr(s->expr);
        set_var(s->var, v);
        return;
      }
      case StmtKind::ExprOnly: {
        (void)eval_expr(s->expr);
        return;
      }
      case StmtKind::Print: {
        Value v = eval_expr(s->expr);
        cout << v.to_string() << "\n";
        return;
      }
      case StmtKind::Return: {
        Value v = eval_expr(s->expr);
        throw ReturnSignal{v};
      }
      case StmtKind::If: {
        Value c = eval_expr(s->cond);
        if(c.truthy()) exec_block(s->thenBody);
        else exec_block(s->elseBody);
        return;
      }
      case StmtKind::FuncDef: {
        env.funcs[s->fname] = Function{s->params, s->body};
        return;
      }
    }
  }

  void exec_block(const vector<StmtPtr>& body){
    for(auto& st: body){
      exec_stmt(st);
    }
  }

  void load_program(const vector<StmtPtr>& prog){
    // First pass: function defs register; other top-level statements execute once at boot.
    for(auto& st: prog){
      if(st->kind==StmtKind::FuncDef) exec_stmt(st);
    }
    for(aut
