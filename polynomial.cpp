#include <iostream>
#include <vector>

using namespace std;

template <typename T>
class Polynomial {
    private:
        vector<T> data;

    public:
        Polynomial() {
            data = vector<T>(1, 0);
        }

        Polynomial(const vector<T>& v)
            : data(v) {
        }

        Polynomial(T coef) {
            data = vector<T>(1, coef);
        }

        template <typename InputIterator>
        Polynomial(InputIterator first, InputIterator last) {
            for (auto it = first; it != last; ++it) {
                data.push_back(*it);
            }
        }

        const auto begin() const {
            return data.begin();
        }

        const auto end() const {
            return data.end();
        }

        bool operator == (const Polynomial<T>& other) const {
            Polynomial<T> p1 = other, p2 = *this;
            p1.delete_zeros();
            p2.delete_zeros();
            if (p1.data.size() != p2.data.size()) {
                return false;
            }
            for (size_t i = 0; i != p1.data.size(); ++i) {
                if (p1.data[i] != p2.data[i]) {
                    return false;
                }
            }
            return true;
        }

        bool operator == (T other) const {
            Polynomial<T> p = *this;
            p.delete_zeros();
            if (p.Degree() > 0) {
                return false;
            } else if (p.Degree() == -1) {
                if (other == T(0)) {
                    return true;
                }
                return false;
            }
            return p[0] == other;
        }

        bool operator != (const Polynomial<T>& other) const {
            return !(*this == other);
        }

        bool operator != (T other) const {
            return !(*this == other);
        }

        void delete_zeros() {
            while (!data.empty() && data[data.size() - 1] == T(0)) {
                data.pop_back();
            }
        }

        Polynomial<T> operator + (const Polynomial<T>& other) const {
            vector<T> v(max(data.size(), other.data.size()));
            Polynomial<T> res(v);
            for (size_t i = 0; i != res.data.size(); ++i) {
                if (i >= data.size()) {
                    res.data[i] = other.data[i];
                } else if (i >= other.data.size()) {
                    res.data[i] = data[i];
                } else {
                    res.data[i] = other.data[i] + data[i];
                }
            }
            res.delete_zeros();
            return res;
        }

        Polynomial<T> operator + (T other) const {
            Polynomial<T> res(data);
            if (res.data.empty()) {
                res.data.push_back(other);
            } else {
                res.data[0] += other;
            }
            res.delete_zeros();
            return res;
        }

        Polynomial<T>& operator += (const Polynomial<T>& other) {
            *this = *this + other;
            return *this;
        }

        Polynomial<T>& operator += (T other) {
            *this = *this + other;
            return *this;
        }

        Polynomial<T> operator * (T other) const {
            Polynomial<T> res(data);
            for (size_t i = 0; i != data.size(); ++i) {
                res.data[i] *= other;
            }
            res.delete_zeros();
            return res;
        }

        Polynomial<T> operator * (const Polynomial<T>& other) const {
            vector<T> v(data.size() + other.data.size(), T(0));
            Polynomial<T> res(v);
            for (size_t i = 0; i != data.size(); ++i) {
                for (size_t j = 0; j != other.data.size(); ++j) {
                    res.data[i + j] += data[i] * other.data[j];
                }
            }
            res.delete_zeros();
            return res;
        }

        Polynomial<T>& operator *= (const T other) {
            *this = *this * other;
            return *this;
        }

        Polynomial<T>& operator *= (const Polynomial<T>& other) {
            *this = *this * other;
            return *this;
        }

        Polynomial<T> operator - (const Polynomial<T>& other) const {
            Polynomial res = *this + other * T(-1);
            return res;
        }

        Polynomial<T> operator - (const T other) {
            Polynomial res = *this + other * T(-1);
            return res;
        }

        Polynomial<T>& operator -= (const Polynomial<T>& other) {
            *this = *this - other;
            return *this;
        }

        Polynomial<T>& operator -= (const T other) {
            *this = *this - other;
            return *this;
        }

        T operator[] (size_t i) const {
            if (i >= data.size()) {
                return 0;
            }
            return data[i];
        }

        int Degree() const {
            if (data.empty()) {
                return -1;
            }
            size_t degree = data.size() - 1;
            while (degree != 0 && data[degree] == T(0)) {
                degree -= 1;
            }
            if (data[degree] == T(0)) {
                return degree - 1;
            }
            return degree;
        }

        T operator () (const T parameter) const {
            T res = 0;
            for (auto it = data.rbegin(); it != data.rend(); ++it) {
                res *= parameter;
                res += *it;
            }
            return res;
        }

        Polynomial<T> operator / (const Polynomial<T>& other) const {
            vector<T> v(data.size() - other.data.size() + 1, T(0));
            Polynomial<T> p = *this;
            Polynomial<T> g = other;
            p.delete_zeros();
            g.delete_zeros();
            while (p.Degree() >= g.Degree() && (p.Degree() != -1)) {
                T x = p[p.data.size() - 1] / g[g.data.size() - 1];
                v[p.data.size() - g.data.size()] = x;
                vector<T> tmp(p.data.size() - g.data.size() + 1, T(0));
                tmp[tmp.size() - 1] = x;
                Polynomial<T> tmp_p(tmp);
                p -= (g * tmp_p);
            }
            Polynomial<T> res(v);
            res.delete_zeros();
            return res;
        }

        Polynomial<T> operator % (const Polynomial<T>& other) const {
            Polynomial<T> res = *this - (*this / other) * other;
            return res;
        }

        Polynomial<T> operator , (const Polynomial<T>& other) const {
            Polynomial<T> first_p(0), second_p(0);
            if ((*this).Degree() > other.Degree()) {
                first_p = *this;
                second_p = other;
            } else {
                first_p = other;
                second_p = *this;
            }
            first_p.delete_zeros();
            second_p.delete_zeros();
            while (second_p != 0) {
                Polynomial<T> tmp = first_p;
                first_p = second_p;
                second_p = tmp % second_p;
            }
            return first_p;
        }

        Polynomial<T> operator & (const Polynomial<T>& other) const {
            Polynomial<T> f = *this;
            Polynomial<T> g = other;
            f.delete_zeros();
            g.delete_zeros();
            Polynomial<T> res(0);
            for (size_t i = 0; i < f.data.size(); ++i) {
                res *= g;
                res += f[f.data.size() - i - 1];
            }
            return res;
        }
};

template <typename T>
ostream& operator << (ostream& out, const Polynomial<T>& polynomial) {
    Polynomial<T> p = polynomial;
    p.delete_zeros();
    if (p.Degree() == -1) {
        out << T(0);
        return out;
    }
    for (size_t i = 0; i != static_cast<size_t>(p.Degree() + 1); ++i) {
        size_t cur_index = static_cast<size_t>(p.Degree() + 1) - i - 1;
        if (p[cur_index] != T(0)) {
            if (p[cur_index] < T(0)) {
                if (p[cur_index] != T(-1)) {
                    out << p[cur_index];
                    if (cur_index != 0) {
                        cout << '*';
                    }
                } else {
                    out << '-';
                }
            } else {
                if (cur_index != static_cast<size_t>(p.Degree())) {
                    out << '+';
                }
                if (p[cur_index] != T(1)) {
                    out << p[cur_index];
                    if (cur_index != 0) {
                        cout << '*';
                    }
                }
            }
            if (cur_index > 1) {
                out << "x^" << cur_index;
            } else if (cur_index == 1) {
                out << 'x';
            } else if (p[cur_index] == T(-1) || p[cur_index] == T(1)) {
                out << T(1);
            }
        }
    }
    return out;
}
