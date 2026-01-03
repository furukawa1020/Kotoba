// kotoba_app_ext.cpp
// Kotoba App UI Extension Layer
// Reads JSON lines from stdin, normalizes UI schema, outputs final JSON.
//
// Build:
//   clang++ -std=c++20 -O2 kotoba_app_ext.cpp -o kotoba_ui
//
// Use (pipe):
//   ./kotoba_app app.kb | ./kotoba_ui

#include <bits/stdc++.h>
using namespace std;

// very small JSON helper (assumes valid, flat input from kotoba_app)
static string escape(const string& s){
  string o;
  for(char c: s){
    if(c=='"') o+="\\\"";
    else if(c=='\\') o+="\\\\";
    else if(c=='\n') o+="\\n";
    else o.push_back(c);
  }
  return o;
}

int main(){
  ios::sync_with_stdio(false);
  cin.tie(nullptr);

  string line;
  while(getline(cin, line)){
    if(line.empty()){
      cout << "{\"text\":\"\",\"input\":null,\"buttons\":[]}\n";
      continue;
    }

    // If already full UI JSON, just pass through
    if(line.find("\"input\"") != string::npos){
      cout << line << "\n";
      continue;
    }

    // Otherwise, wrap legacy output into full UI schema
    // legacy: {"text":"...","buttons":[...]}
    // new:
    // {
    //   text,
    //   input:{type:"text",placeholder:""},
    //   buttons
    // }

    // crude extraction (safe because kotoba_app controls output)
    string text;
    vector<string> buttons;

    auto tpos = line.find("\"text\":\"");
    if(tpos != string::npos){
      tpos += 8;
      auto tend = line.find("\"", tpos);
      text = line.substr(tpos, tend - tpos);
    }

    auto bpos = line.find("\"buttons\":[");
    if(bpos != string::npos){
      bpos += 11;
      auto bend = line.find("]", bpos);
      string bs = line.substr(bpos, bend - bpos);
      string cur;
      bool in=false;
      for(char c: bs){
        if(c=='"'){
          if(in){
            buttons.push_back(cur);
            cur.clear();
          }
          in = !in;
        } else if(in){
          cur.push_back(c);
        }
      }
    }

    // emit normalized UI JSON
    cout
      << "{"
      << "\"text\":\"" << escape(text) << "\","
      << "\"input\":{\"type\":\"text\",\"placeholder\":\"\"},"
      << "\"buttons\":[";
    for(size_t i=0;i<buttons.size();i++){
      if(i) cout << ",";
      cout << "\"" << escape(buttons[i]) << "\"";
    }
    cout << "]}"
    << "\n";
  }
  return 0;
}
