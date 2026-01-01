// Kotoba（ことば） v0 - single-file “compiler + bytecode VM” implementation
//
// Build (Termux / Linux / macOS):
//   clang++ -std=c++20 -O2 -Wall -Wextra kotoba.cpp -o kotoba
//
// Run:
//   ./kotoba examples/hello.kb
//
// Language (v0):
// - Statement per line (NEWLINE separates statements)
// - Blocks: ":" starts block, "おわり" ends block
// - Keywords (hiragana):
//     つくる (function)   かえす (return)   だす (print)
//     もし (if)           ほか (else)
//     くり (for-range)    を / から / まで
// - Assignment: 変数 = 式
// - Expressions: + - * /, comparisons: == != < <= > >=
// - Literals: numbers, strings "..."
//
// Example:
//
// つくる たす(a,b):
//   かえす a + b
// おわり
//
// x = たす(1,2)
// だす x
//

#include <bits/stdc++.h>
using namespace std;

// -------------------- Value --------------------
struct Value {
  enum class Type { NIL, NUMBER, STRING, BOOL } type = Type::NIL;
  double num = 0.0;
  std::string str;
  bool b = false;

  static Value Nil() { return {}; }
  static Value Num(double v) { Value x; x.type=Type::NUMBER; x.num=v; return x; }
  static Value Str(std::string v) { Value x; x.type=Type::STRING; x.str=std::move(v); return x; }
  static Value Bool(bool v) { Value x; x.type=Type::BOOL; x.b=v; return x; }

  std::string toString() const {
    switch(type){
      case Type::NIL: return "なし";
      case Type::NUMBER: {
        // pretty print
        std::ostringstream oss;
        oss << std::setprecision(15) << num;
        auto s = oss.str();
        // trim trailing zeros
        if (s.find('.') != std::string::npos) {
          while(!s.empty() && s.back()=='0') s.pop_back();
          if(!s.empty() && s.back()=='.') s.pop_back();
        }
        return s;
      }
      case Type::STRING: return str;
      case Type::BOOL: return b ? "真" : "偽";
    }
    return "なし";
  }

  bool truthy() const {
    switch(type){
      case Type::NIL: return false;
      case Type::BOOL: return b;
      case Type::NUMBER: return num != 0.0;
      case Type::STRING: return !str.empty();
    }
    return false;
  }
};

static bool valueEq(const Value& a, const Value& b){
  if (a.type != b.type) return false;
  switch(a.type){
    case Value::Type::NIL: return true;
    case Value::Type::NUMBER: return a.num == b.num;
    case Value::Type::STRING: return a.str == b.str;
    case Value::Type::BOOL: return a.b == b.b;
  }
  return false;
}

// -------------------- Errors --------------------
struct KError : std::runtime_error {
  int line=0, col=0;
  KError(int l,int c,const std::string& m): std::runtime_error(m), line(l), col(c) {}
};

// -------------------- Tokenizer --------------------
enum class TokKind {
  // single-char
  LPAREN, RPAREN, COMMA, COLON,
  PLUS, MINUS, STAR, SLASH,
  ASSIGN,
  NEWLINE, EOF_TOK,

  // multi-char ops
  EQ, NEQ, LT, LTE, GT, GTE,

  // literals / ids
  NUMBER, STRING, IDENT,

  // keywords
  KW_TUKURU, KW_KAESU, KW_DASU,
  KW_MOSHI, KW_HOKA,
  KW_KURI, KW_WO, KW_KARA, KW_MADE,
  KW_OWARI
};

struct Token {
  TokKind kind;
  std::string lex;
  double num = 0.0;
  int line=1, col=1;
};

static bool isAsciiSpace(char c){ return c==' '||c=='\t'||c=='\r'; }

struct Lexer {
  std::string src;
  size_t i=0;
  int line=1, col=1;

  explicit Lexer(std::string s): src(std::move(s)) {}

  char peek() const { return i<src.size()? src[i] : '\0'; }
  char peek2() const { return (i+1<src.size())? src[i+1] : '\0'; }

  char get() {
    char c = peek();
    if (c=='\0') return c;
    i++;
    if (c=='\n'){ line++; col=1; }
    else col++;
    return c;
  }

  void skipSpaces(){
    while(true){
      char c = peek();
      if (isAsciiSpace(c)) { get(); continue; }
      // comment: # ... endline
      if (c=='#') {
        while(peek()!='\0' && peek()!='\n') get();
        continue;
      }
      break;
    }
  }

  Token make(TokKind k, std::string lx, int l, int c){
    Token t{k, std::move(lx), 0.0, l, c};
    return t;
  }

  static TokKind keywordKind(const std::string& s){
    if (s=="つくる") return TokKind::KW_TUKURU;
    if (s=="かえす") return TokKind::KW_KAESU;
    if (s=="だす")   return TokKind::KW_DASU;
    if (s=="もし")   return TokKind::KW_MOSHI;
    if (s=="ほか")   return TokKind::KW_HOKA;
    if (s=="くり")   return TokKind::KW_KURI;
    if (s=="を")     return TokKind::KW_WO;
    if (s=="から")   return TokKind::KW_KARA;
    if (s=="まで")   return TokKind::KW_MADE;
    if (s=="おわり") return TokKind::KW_OWARI;
    return TokKind::IDENT;
  }

  // read UTF-8-ish identifier chunk: accept anything except ASCII delimiters/operators/newlines/quotes
  bool isIdentChar(char c) const {
    if (c=='\0'||c=='\n') return false;
    // ASCII delimiters/operators
    string delims = " \t\r()=,:+-*/<>!\"#";
    if (delims.find(c)!=string::npos) return false;
    return true; // including multi-byte UTF-8 bytes (>=0x80)
  }

  Token next(){
    skipSpaces();
    int l=line, c=col;
    char ch = peek();
    if (ch=='\0') return make(TokKind::EOF_TOK,"",l,c);

    if (ch=='\n'){ get(); return make(TokKind::NEWLINE,"\\n",l,c); }

    // multi-char ops
    if (ch=='=' && peek2()=='='){ get(); get(); return make(TokKind::EQ,"==",l,c); }
    if (ch=='!' && peek2()=='='){ get(); get(); return make(TokKind::NEQ,"!=",l,c); }
    if (ch=='<' && peek2()=='='){ get(); get(); return make(TokKind::LTE,"<=",l,c); }
    if (ch=='>' && peek2()=='='){ get(); get(); return make(TokKind::GTE,">=",l,c); }

    // single-char
    switch(ch){
      case '(': get(); return make(TokKind::LPAREN,"(",l,c);
      case ')': get(); return make(TokKind::RPAREN,")",l,c);
      case ',': get(); return make(TokKind::COMMA,",",l,c);
      case ':': get(); return make(TokKind::COLON,":",l,c);
      case '+': get(); return make(TokKind::PLUS,"+",l,c);
      case '-': get(); return make(TokKind::MINUS,"-",l,c);
      case '*': get(); return make(TokKind::STAR,"*",l,c);
      case '/': get(); return make(TokKind::SLASH,"/",l,c);
      case '=': get(); return make(TokKind::ASSIGN,"=",l,c);
      case '<': get(); return make(TokKind::LT,"<",l,c);
      case '>': get(); return make(TokKind::GT,">",l,c);
      case '"': {
        get(); // "
        std::string out;
        while(true){
          char cc = peek();
          if (cc=='\0' || cc=='\n') throw KError(l,c,"文字列が閉じていません");
          if (cc=='"'){ get(); break; }
          if (cc=='\\'){
            get();
            char e = get();
            if (e=='n') out.push_back('\n');
            else if (e=='t') out.push_back('\t');
            else if (e=='"') out.push_back('"');
            else if (e=='\\') out.push_back('\\');
            else out.push_back(e);
          } else {
            out.push_back(get());
          }
        }
        Token t = make(TokKind::STRING,out,l,c);
        t.lex = out;
        return t;
      }
    }

    // number (ASCII)
    if (isdigit((unsigned char)ch) || (ch=='.' && isdigit((unsigned char)peek2()))){
      std::string s;
      bool dot=false;
      while(true){
        char cc = peek();
        if (isdigit((unsigned char)cc)) { s.push_back(get()); continue; }
        if (cc=='.' && !dot){ dot=true; s.push_back(get()); continue; }
        break;
      }
      Token t = make(TokKind::NUMBER,s,l,c);
      t.num = stod(s);
      return t;
    }

    // identifier / keyword (includes UTF-8)
    if (isIdentChar(ch)){
      std::string s;
      while(isIdentChar(peek())) s.push_back(get());
      TokKind kk = keywordKind(s);
      Token t = make(kk,s,l,c);
      t.lex = s;
      return t;
    }

    // unknown
    std::string msg = "不明な文字: ";
    msg.push_back(ch);
    throw KError(l,c,msg);
  }
};

// -------------------- Parser (AST) --------------------
struct Expr {
  struct Bin { std::string op; unique_ptr<Expr> l,r; };
  struct Unary { std::string op; unique_ptr<Expr> e; };
  struct Lit { Value v; };
  struct Var { std::string name; };
  struct Call { std::string name; vector<unique_ptr<Expr>> args; };

  variant<Bin,Unary,Lit,Var,Call> node;
  int line=1, col=1;
};

struct Stmt {
  struct Print { unique_ptr<Expr> e; };
  struct Return { unique_ptr<Expr> e; };
  struct Assign { std::string name; unique_ptr<Expr> e; };
  struct ExprStmt { unique_ptr<Expr> e; };
  struct If { unique_ptr<Expr> cond; vector<unique_ptr<Stmt>> thenS; vector<unique_ptr<Stmt>> elseS; bool hasElse=false; };
  struct For { std::string var; unique_ptr<Expr> start; unique_ptr<Expr> end; vector<unique_ptr<Stmt>> body; };
  struct Func { std::string name; vector<std::string> params; vector<unique_ptr<Stmt>> body; };

  variant<Print,Return,Assign,ExprStmt,If,For,Func> node;
  int line=1, col=1;
};

struct Parser {
  vector<Token> t;
  size_t p=0;

  explicit Parser(vector<Token> toks): t(std::move(toks)) {}

  const Token& peek() const { return t[p]; }
  const Token& prev() const { return t[p-1]; }
  bool is(TokKind k) const { return peek().kind==k; }
  const Token& eat(TokKind k, const string& msg){
    if (!is(k)) throw KError(peek().line, peek().col, msg);
    return t[p++];
  }
  bool match(TokKind k){
    if (is(k)) { p++; return true; }
    return false;
  }
  void skipNewlines(){ while(match(TokKind::NEWLINE)){} }

  // ---- expressions ----
  unique_ptr<Expr> parseExpr(){ return parseEquality(); }

  unique_ptr<Expr> parseEquality(){
    auto e = parseComparison();
    while(true){
      if (match(TokKind::EQ)){
        auto r = parseComparison();
        auto x = make_unique<Expr>(); x->line=prev().line; x->col=prev().col;
        x->node = Expr::Bin{"==", std::move(e), std::move(r)};
        e = std::move(x);
      } else if (match(TokKind::NEQ)){
        auto r = parseComparison();
        auto x = make_unique<Expr>(); x->line=prev().line; x->col=prev().col;
        x->node = Expr::Bin{"!=", std::move(e), std::move(r)};
        e = std::move(x);
      } else break;
    }
    return e;
  }

  unique_ptr<Expr> parseComparison(){
    auto e = parseTerm();
    while(true){
      if (match(TokKind::LT) || match(TokKind::LTE) || match(TokKind::GT) || match(TokKind::GTE)){
        string op = prev().lex;
        auto r = parseTerm();
        auto x = make_unique<Expr>(); x->line=prev().line; x->col=prev().col;
        x->node = Expr::Bin{op, std::move(e), std::move(r)};
        e = std::move(x);
      } else break;
    }
    return e;
  }

  unique_ptr<Expr> parseTerm(){
    auto e = parseFactor();
    while(true){
      if (match(TokKind::PLUS) || match(TokKind::MINUS)){
        string op = prev().lex;
        auto r = parseFactor();
        auto x = make_unique<Expr>(); x->line=prev().line; x->col=prev().col;
        x->node = Expr::Bin{op, std::move(e), std::move(r)};
        e = std::move(x);
      } else break;
    }
    return e;
  }

  unique_ptr<Expr> parseFactor(){
    auto e = parseUnary();
    while(true){
      if (match(TokKind::STAR) || match(TokKind::SLASH)){
        string op = prev().lex;
        auto r = parseUnary();
        auto x = make_unique<Expr>(); x->line=prev().line; x->col=prev().col;
        x->node = Expr::Bin{op, std::move(e), std::move(r)};
        e = std::move(x);
      } else break;
    }
    return e;
  }

  unique_ptr<Expr> parseUnary(){
    if (match(TokKind::MINUS)){
      auto e = parseUnary();
      auto x = make_unique<Expr>(); x->line=prev().line; x->col=prev().col;
      x->node = Expr::Unary{"-", std::move(e)};
      return x;
    }
    return parsePrimary();
  }

  unique_ptr<Expr> parsePrimary(){
    const Token& tk = peek();
    if (match(TokKind::NUMBER)){
      auto x = make_unique<Expr>(); x->line=tk.line; x->col=tk.col;
      x->node = Expr::Lit{Value::Num(tk.num)};
      return x;
    }
    if (match(TokKind::STRING)){
      auto x = make_unique<Expr>(); x->line=tk.line; x->col=tk.col;
      x->node = Expr::Lit{Value::Str(tk.lex)};
      return x;
    }
    if (match(TokKind::IDENT)){
      string name = tk.lex;
      // call?
      if (match(TokKind::LPAREN)){
        vector<unique_ptr<Expr>> args;
        if (!is(TokKind::RPAREN)){
          do { args.push_back(parseExpr()); } while(match(TokKind::COMMA));
        }
        eat(TokKind::RPAREN, "`)` が必要です");
        auto x = make_unique<Expr>(); x->line=tk.line; x->col=tk.col;
        x->node = Expr::Call{name, std::move(args)};
        return x;
      } else {
        auto x = make_unique<Expr>(); x->line=tk.line; x->col=tk.col;
        x->node = Expr::Var{name};
        return x;
      }
    }
    if (match(TokKind::LPAREN)){
      auto e = parseExpr();
      eat(TokKind::RPAREN, "`)` が必要です");
      return e;
    }
    throw KError(tk.line, tk.col, "式が必要です");
  }

  // ---- statements ----
  vector<unique_ptr<Stmt>> parseBlock(){
    // expects: after ":" NEWLINE already consumed by caller maybe
    vector<unique_ptr<Stmt>> out;
    skipNewlines();
    while(!is(TokKind::KW_OWARI) && !is(TokKind::EOF_TOK) && !is(TokKind::KW_HOKA)){
      out.push_back(parseStmt());
      skipNewlines();
    }
    return out;
  }

  unique_ptr<Stmt> parseFunc(){
    const auto& kw = eat(TokKind::KW_TUKURU, "`つくる` が必要です");
    Token nameTok = eat(TokKind::IDENT, "関数名が必要です");
    eat(TokKind::LPAREN, "`(` が必要です");
    vector<string> params;
    if (!is(TokKind::RPAREN)){
      do {
        Token pTok = eat(TokKind::IDENT, "引数名が必要です");
        params.push_back(pTok.lex);
      } while(match(TokKind::COMMA));
    }
    eat(TokKind::RPAREN, "`)` が必要です");
    eat(TokKind::COLON, "`:` が必要です");
    // block
    auto body = parseBlock();
    eat(TokKind::KW_OWARI, "`おわり` が必要です（関数）");

    auto s = make_unique<Stmt>(); s->line=kw.line; s->col=kw.col;
    s->node = Stmt::Func{nameTok.lex, std::move(params), std::move(body)};
    return s;
  }

  unique_ptr<Stmt> parseIf(){
    const auto& kw = eat(TokKind::KW_MOSHI, "`もし` が必要です");
    auto cond = parseExpr();
    eat(TokKind::COLON, "`:` が必要です（もし）");
    auto thenS = parseBlock();

    vector<unique_ptr<Stmt>> elseS;
    bool hasElse=false;
    if (match(TokKind::KW_HOKA)){
      hasElse=true;
      eat(TokKind::COLON, "`:` が必要です（ほか）");
      elseS = parseBlock();
    }

    eat(TokKind::KW_OWARI, "`おわり` が必要です（もし）");

    auto s = make_unique<Stmt>(); s->line=kw.line; s->col=kw.col;
    s->node = Stmt::If{std::move(cond), std::move(thenS), std::move(elseS), hasElse};
    return s;
  }

  unique_ptr<Stmt> parseFor(){
    const auto& kw = eat(TokKind::KW_KURI, "`くり` が必要です");
    Token varTok = eat(TokKind::IDENT, "ループ変数が必要です");
    eat(TokKind::KW_WO, "`を` が必要です");
    auto start = parseExpr();
    eat(TokKind::KW_KARA, "`から` が必要です");
    auto end = parseExpr();
    eat(TokKind::KW_MADE, "`まで` が必要です");
    eat(TokKind::COLON, "`:` が必要です（くり）");
    auto body = parseBlock();
    eat(TokKind::KW_OWARI, "`おわり` が必要です（くり）");

    auto s = make_unique<Stmt>(); s->line=kw.line; s->col=kw.col;
    s->node = Stmt::For{varTok.lex, std::move(start), std::move(end), std::move(body)};
    return s;
  }

  unique_ptr<Stmt> parseStmt(){
    skipNewlines();
    const Token& tk = peek();

    if (is(TokKind::KW_TUKURU)) return parseFunc();
    if (is(TokKind::KW_MOSHI)) return parseIf();
    if (is(TokKind::KW_KURI))  return parseFor();

    if (match(TokKind::KW_DASU)){
      auto e = parseExpr();
      auto s = make_unique<Stmt>(); s->line=tk.line; s->col=tk.col;
      s->node = Stmt::Print{std::move(e)};
      return s;
    }
    if (match(TokKind::KW_KAESU)){
      // return expr optional? (we'll require expr for v0)
      auto e = parseExpr();
      auto s = make_unique<Stmt>(); s->line=tk.line; s->col=tk.col;
      s->node = Stmt::Return{std::move(e)};
      return s;
    }

    // assignment or exprstmt
    if (is(TokKind::IDENT) && t[p+1].kind==TokKind::ASSIGN){
      Token nameTok = eat(TokKind::IDENT, "変数名が必要です");
      eat(TokKind::ASSIGN, "`=` が必要です");
      auto e = parseExpr();
      auto s = make_unique<Stmt>(); s->line=nameTok.line; s->col=nameTok.col;
      s->node = Stmt::Assign{nameTok.lex, std::move(e)};
      return s;
    }

    // expression statement
    auto e = parseExpr();
    auto s = make_unique<Stmt>(); s->line=tk.line; s->col=tk.col;
    s->node = Stmt::ExprStmt{std::move(e)};
    return s;
  }

  vector<unique_ptr<Stmt>> parseProgram(){
    vector<unique_ptr<Stmt>> out;
    skipNewlines();
    while(!is(TokKind::EOF_TOK)){
      out.push_back(parseStmt());
      skipNewlines();
    }
    return out;
  }
};

// -------------------- Bytecode --------------------
enum class Op : uint8_t {
  PUSH_CONST,
  LOAD, STORE,
  ADD, SUB, MUL, DIV,
  EQ, NEQ, LT, LTE, GT, GTE,
  NEG,
  PRINT,
  JUMP,
  JUMP_IF_FALSE,
  CALL,
  RETURN,
  POP
};

struct Instr {
  Op op;
  int a=0; // general operand (index / jump target / arg count)
  int b=0;
};

struct FunctionBC {
  vector<string> params;
  vector<Instr> code;
  vector<Value> consts;
};

struct ProgramBC {
  // global "main" code runs first
  vector<Instr> mainCode;
  vector<Value> mainConsts;
  unordered_map<string, FunctionBC> funcs;
};

// -------------------- Compiler --------------------
struct Compiler {
  ProgramBC prog;

  int addConst(vector<Value>& pool, Value v){
    pool.push_back(std::move(v));
    return (int)pool.size()-1;
  }

  void emit(vector<Instr>& code, Op op, int a=0, int b=0){
    code.push_back({op,a,b});
  }

  // compile expr into stack machine
  void cExpr(const Expr& e, vector<Instr>& code, vector<Value>& consts){
    if (holds_alternative<Expr::Lit>(e.node)){
      const auto& n = get<Expr::Lit>(e.node);
      int idx = addConst(consts, n.v);
      emit(code, Op::PUSH_CONST, idx);
      return;
    }
    if (holds_alternative<Expr::Var>(e.node)){
      const auto& n = get<Expr::Var>(e.node);
      // LOAD by name index in const pool (store name as string const)
      int idx = addConst(consts, Value::Str(n.name));
      emit(code, Op::LOAD, idx);
      return;
    }
    if (holds_alternative<Expr::Unary>(e.node)){
      const auto& n = get<Expr::Unary>(e.node);
      cExpr(*n.e, code, consts);
      if (n.op=="-") emit(code, Op::NEG);
      else throw KError(e.line,e.col,"未対応の単項演算子: "+n.op);
      return;
    }
    if (holds_alternative<Expr::Bin>(e.node)){
      const auto& n = get<Expr::Bin>(e.node);
      cExpr(*n.l, code, consts);
      cExpr(*n.r, code, consts);
      if (n.op=="+") emit(code, Op::ADD);
      else if (n.op=="-") emit(code, Op::SUB);
      else if (n.op=="*") emit(code, Op::MUL);
      else if (n.op=="/") emit(code, Op::DIV);
      else if (n.op=="==") emit(code, Op::EQ);
      else if (n.op=="!=") emit(code, Op::NEQ);
      else if (n.op=="<") emit(code, Op::LT);
      else if (n.op=="<=") emit(code, Op::LTE);
      else if (n.op==">") emit(code, Op::GT);
      else if (n.op==">=") emit(code, Op::GTE);
      else throw KError(e.line,e.col,"未対応の二項演算子: "+n.op);
      return;
    }
    if (holds_alternative<Expr::Call>(e.node)){
      const auto& n = get<Expr::Call>(e.node);
      for (auto& a : n.args) cExpr(*a, code, consts);
      int nameIdx = addConst(consts, Value::Str(n.name));
      emit(code, Op::CALL, nameIdx, (int)n.args.size());
      return;
    }
    throw KError(e.line,e.col,"式コンパイル失敗");
  }

  void cStmt(const Stmt& s, vector<Instr>& code, vector<Value>& consts){
    if (holds_alternative<Stmt::Print>(s.node)){
      auto& n = get<Stmt::Print>(s.node);
      cExpr(*n.e, code, consts);
      emit(code, Op::PRINT);
      return;
    }
    if (holds_alternative<Stmt::Return>(s.node)){
      auto& n = get<Stmt::Return>(s.node);
      cExpr(*n.e, code, consts);
      emit(code, Op::RETURN);
      return;
    }
    if (holds_alternative<Stmt::Assign>(s.node)){
      auto& n = get<Stmt::Assign>(s.node);
      cExpr(*n.e, code, consts);
      int nameIdx = addConst(consts, Value::Str(n.name));
      emit(code, Op::STORE, nameIdx);
      return;
    }
    if (holds_alternative<Stmt::ExprStmt>(s.node)){
      auto& n = get<Stmt::ExprStmt>(s.node);
      cExpr(*n.e, code, consts);
      emit(code, Op::POP);
      return;
    }
    if (holds_alternative<Stmt::If>(s.node)){
      auto& n = get<Stmt::If>(s.node);
      cExpr(*n.cond, code, consts);
      // ju
