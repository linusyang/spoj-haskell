// Copyright (c) 2015 Linus Yang

#include <string>
#include <iostream>

using namespace std;

int process(char *s, int pl, int pr)
{
    if (pl < 0) {
        return 1;
    } else if (s[pl] < '9') {
        s[pl] = s[pr] = s[pl] + 1;
        return 0;
    } else {
        s[pl] = s[pr] = '0';
        return process(s, pl - 1, pr + 1);
    }
}

void palinize_list(char *s, int n)
{
    int pl, pr;
    pr = n / 2;
    pl = n % 2 ? pr : pr - 1;
    while (pl >= 0 && pr < n) {
        if (s[pl] != s[pr]) {
            s[pr] = s[pl];
        }
        pl--;
        pr++;
    }
}

void next(string& str)
{
    int pl, pr;
    int ret = 0;

    int n = str.size();
    char *s = new char[n + 1];
    std::copy(str.begin(), str.end(), s);
    s[n] = '\0';

    pr = n / 2;
    pl = n % 2 ? pr : pr - 1;
    ret = process(s, pl, pr);
    if (ret) {
        s[n - 1] = '1';
        cout << 1;
    } else {
        palinize_list(s, n);
    }
    cout << s << endl;
    delete [] s;
}

bool can_palinize(string& str)
{
    int n = str.size();
    const char *s = str.c_str();

    int pl, pr;
    pr = n / 2;
    pl = n % 2 ? pr : pr - 1;

    while (pl >= 0 && pr < n) {
        if (s[pl] > s[pr]) {
            return true;
        } else if (s[pl] < s[pr]) {
            return false;
        }
        pl--;
        pr++;
    }
    return false;
}

string palinize(string &str)
{
    int n = str.size();
    char *s = new char[n + 1];
    std::copy(str.begin(), str.end(), s);
    s[n] = '\0';
    palinize_list(s, n);
    string result(s);
    delete [] s;
    return result;
}

int main(void)
{
    int i, n;
    string str;

    cin >> n;
    for (i = 0; i < n; i++) {
        cin >> str;
        if (can_palinize(str)) {
            cout << palinize(str) << endl;
        } else {
            next(str);
        }
    }
    return 0;
}
