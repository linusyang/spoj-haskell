// Copyright (c) 2015 Linus Yang

#include <iostream>
#include <string>
#include <deque>
#include <unordered_map>

using namespace std;

#define ENDSTR "0000000000"
#define START "123456789"
#define CANGO(s, x) (s.find(x) == s.end())
#define MAXD 10

struct qstat {
    int depth;
    string stat;
    
    qstat(int d, string s): depth(d), stat(s) {}
};

string rotate(int d, int clockwise, string& t);

int search_depth(int limit, string init, string dest, unordered_map<string, int>& visited)
{
    if (limit < 0) {
        return -1;
    }
    
    if (limit == 0) {
        return init == dest ? 0 : -1;
    }
    
    deque<qstat> q;
    q.push_back(qstat(0, init));
    visited.insert(make_pair(init, 0));
    while (!q.empty()) {
        qstat st = q.front();
        q.pop_front();
        if (st.stat == dest) {
            return st.depth;
        }
        int next_depth = st.depth + 1;
        if (next_depth > limit) {
            continue;
        }
        string next_stat;
        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < 2; j++) {
                next_stat = rotate(i, j, st.stat);
                if (CANGO(visited, next_stat)) {
                    q.push_back(qstat(next_depth, next_stat));
                    visited.insert(make_pair(next_stat, next_depth));
                }
            }
        }
    }
    return -1;
}

int solve(int limit, string init)
{
    unordered_map<string, int> top_set;
    unordered_map<string, int> bottom_set;
    int top_limit = limit / 2;
    int bottom_limit = limit - top_limit;
    string dest(START);
    
    int top_result = search_depth(top_limit, init, dest, top_set);
    if (top_result >= 0) {
        return top_result;
    }
    
    int bottom_result = search_depth(bottom_limit, dest, init, bottom_set);
    if (bottom_result >= 0) {
        return bottom_result;
    }
    
    int result = MAXD;
    for (auto top_value : top_set) {
        auto bottom_it = bottom_set.find(top_value.first);
        if (bottom_it != bottom_set.end()) {
            int depth = bottom_it -> second + top_value.second;
            if (depth < result && depth <= limit) {
                result = depth;
            }
        }
    }
    
    return result == MAXD ? -1 : result;
}

int main(void)
{
    string s;
    int n = 0;
    do {
        cin >> s;
        if (s == ENDSTR) {
            break;
        }
        cout << ++n << ". " << solve(s[0] - '0', string(s, 1)) << endl;
    } while (true);
    return 0;
}

string rot(string& t, int a, int b, int c, int d)
{
    string s(t);
    char x = s[a];
    s[a] = s[b];
    s[b] = s[c];
    s[c] = s[d];
    s[d] = x;
    return s;
}

string rotate(int d, int clockwise, string& t)
{
    switch (d) {
        case 0:
            return clockwise ? rot(t, 0, 3, 4, 1) : rot(t, 0, 1, 4, 3);
        case 1:
            return clockwise ? rot(t, 1, 4, 5, 2) : rot(t, 1, 2, 5, 4);
        case 2:
            return clockwise ? rot(t, 3, 6, 7, 4) : rot(t, 3, 4, 7, 6);
        case 3:
            return clockwise ? rot(t, 4, 7, 8, 5) : rot(t, 4, 5, 8, 7);
    }
    return t;
}
