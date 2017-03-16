#include <vector>
#include <cstdlib>
#include <iostream>
#include <iomanip>
#include <string>
#include <ctime>
#include <math.h>
using namespace std;

// base and base_digits must be consistent
const int base = 1000000000;
const int base_digits = 9;

struct bigint {
    vector<int> a;
    int sign;

    bigint() :
        sign(1) {
    }

    bigint(long long v) {
        *this = v;
    }

    bigint(const string &s) {
        read(s);
    }

    void operator=(const bigint &v) {
        sign = v.sign;
        a = v.a;
    }

    void operator=(long long v) {
        sign = 1;
        if (v < 0)
            sign = -1, v = -v;
        for (; v > 0; v = v / base)
            a.push_back(v % base);
    }

    bigint operator+(const bigint &v) const {
        if (sign == v.sign) {
            bigint res = v;

            for (int i = 0, carry = 0; i < (int) max(a.size(), v.a.size()) || carry; ++i) {
                if (i == (int) res.a.size())
                    res.a.push_back(0);
                res.a[i] += carry + (i < (int) a.size() ? a[i] : 0);
                carry = res.a[i] >= base;
                if (carry)
                    res.a[i] -= base;
            }
            return res;
        }
        return *this - (-v);
    }

    bigint operator-(const bigint &v) const {
        if (sign == v.sign) {
            if (abs() >= v.abs()) {
                bigint res = *this;
                for (int i = 0, carry = 0; i < (int) v.a.size() || carry; ++i) {
                    res.a[i] -= carry + (i < (int) v.a.size() ? v.a[i] : 0);
                    carry = res.a[i] < 0;
                    if (carry)
                        res.a[i] += base;
                }
                res.trim();
                return res;
            }
            return -(v - *this);
        }
        return *this + (-v);
    }

    void operator*=(int v) {
        if (v < 0)
            sign = -sign, v = -v;
        for (int i = 0, carry = 0; i < (int) a.size() || carry; ++i) {
            if (i == (int) a.size())
                a.push_back(0);
            long long cur = a[i] * (long long) v + carry;
            carry = (int) (cur / base);
            a[i] = (int) (cur % base);
            //asm("divl %%ecx" : "=a"(carry), "=d"(a[i]) : "A"(cur), "c"(base));
        }
        trim();
    }

    bigint operator*(int v) const {
        bigint res = *this;
        res *= v;
        return res;
    }

    friend pair<bigint, bigint> divmod(const bigint &a1, const bigint &b1) {
        int norm = base / (b1.a.back() + 1);
        bigint a = a1.abs() * norm;
        bigint b = b1.abs() * norm;
        bigint q, r;
        q.a.resize(a.a.size());

        for (int i = a.a.size() - 1; i >= 0; i--) {
            r *= base;
            r += a.a[i];
            int s1 = r.a.size() <= b.a.size() ? 0 : r.a[b.a.size()];
            int s2 = r.a.size() <= b.a.size() - 1 ? 0 : r.a[b.a.size() - 1];
            int d = ((long long) base * s1 + s2) / b.a.back();
            r -= b * d;
            while (r < 0)
                r += b, --d;
            q.a[i] = d;
        }

        q.sign = a1.sign * b1.sign;
        r.sign = a1.sign;
        q.trim();
        r.trim();
        return make_pair(q, r / norm);
    }

    bigint operator/(const bigint &v) const {
        return divmod(*this, v).first;
    }

    bigint operator%(const bigint &v) const {
        return divmod(*this, v).second;
    }

    void operator/=(int v) {
        if (v < 0)
            sign = -sign, v = -v;
        for (int i = (int) a.size() - 1, rem = 0; i >= 0; --i) {
            long long cur = a[i] + rem * (long long) base;
            a[i] = (int) (cur / v);
            rem = (int) (cur % v);
        }
        trim();
    }

    bigint operator/(int v) const {
        bigint res = *this;
        res /= v;
        return res;
    }

    int operator%(int v) const {
        if (v < 0)
            v = -v;
        int m = 0;
        for (int i = a.size() - 1; i >= 0; --i)
            m = (a[i] + m * (long long) base) % v;
        return m * sign;
    }

    void operator+=(const bigint &v) {
        *this = *this + v;
    }
    void operator-=(const bigint &v) {
        *this = *this - v;
    }
    void operator*=(const bigint &v) {
        *this = *this * v;
    }
    void operator/=(const bigint &v) {
        *this = *this / v;
    }

    bool operator<(const bigint &v) const {
        if (sign != v.sign)
            return sign < v.sign;
        if (a.size() != v.a.size())
            return a.size() * sign < v.a.size() * v.sign;
        for (int i = a.size() - 1; i >= 0; i--)
            if (a[i] != v.a[i])
                return a[i] * sign < v.a[i] * sign;
        return false;
    }

    bool operator>(const bigint &v) const {
        return v < *this;
    }
    bool operator<=(const bigint &v) const {
        return !(v < *this);
    }
    bool operator>=(const bigint &v) const {
        return !(*this < v);
    }
    bool operator==(const bigint &v) const {
        return !(*this < v) && !(v < *this);
    }
    bool operator!=(const bigint &v) const {
        return *this < v || v < *this;
    }

    void trim() {
        while (!a.empty() && !a.back())
            a.pop_back();
        if (a.empty())
            sign = 1;
    }

    bool isZero() const {
        return a.empty() || (a.size() == 1 && !a[0]);
    }

    bigint operator-() const {
        bigint res = *this;
        res.sign = -sign;
        return res;
    }

    bigint abs() const {
        bigint res = *this;
        res.sign *= res.sign;
        return res;
    }

    long long longValue() const {
        long long res = 0;
        for (int i = a.size() - 1; i >= 0; i--)
            res = res * base + a[i];
        return res * sign;
    }

    friend bigint gcd(const bigint &a, const bigint &b) {
        return b.isZero() ? a : gcd(b, a % b);
    }
    friend bigint lcm(const bigint &a, const bigint &b) {
        return a / gcd(a, b) * b;
    }

    void read(const string &s) {
        sign = 1;
        a.clear();
        int pos = 0;
        while (pos < (int) s.size() && (s[pos] == '-' || s[pos] == '+')) {
            if (s[pos] == '-')
                sign = -sign;
            ++pos;
        }
        for (int i = s.size() - 1; i >= pos; i -= base_digits) {
            int x = 0;
            for (int j = max(pos, i - base_digits + 1); j <= i; j++)
                x = x * 10 + s[j] - '0';
            a.push_back(x);
        }
        trim();
    }

    friend istream& operator>>(istream &stream, bigint &v) {
        string s;
        stream >> s;
        v.read(s);
        return stream;
    }

    friend ostream& operator<<(ostream &stream, const bigint &v) {
        if (v.sign == -1)
            stream << '-';
        stream << (v.a.empty() ? 0 : v.a.back());
        for (int i = (int) v.a.size() - 2; i >= 0; --i)
            stream << setw(base_digits) << setfill('0') << v.a[i];
        return stream;
    }

    static vector<int> convert_base(const vector<int> &a, int old_digits, int new_digits) {
        vector<long long> p(max(old_digits, new_digits) + 1);
        p[0] = 1;
        for (int i = 1; i < (int) p.size(); i++)
            p[i] = p[i - 1] * 10;
        vector<int> res;
        long long cur = 0;
        int cur_digits = 0;
        for (int i = 0; i < (int) a.size(); i++) {
            cur += a[i] * p[cur_digits];
            cur_digits += old_digits;
            while (cur_digits >= new_digits) {
                res.push_back(int(cur % p[new_digits]));
                cur /= p[new_digits];
                cur_digits -= new_digits;
            }
        }
        res.push_back((int) cur);
        while (!res.empty() && !res.back())
            res.pop_back();
        return res;
    }

    typedef vector<long long> vll;

    static vll karatsubaMultiply(const vll &a, const vll &b) {
        int n = a.size();
        vll res(n + n);
        if (n <= 32) {
            for (int i = 0; i < n; i++)
                for (int j = 0; j < n; j++)
                    res[i + j] += a[i] * b[j];
            return res;
        }

        int k = n >> 1;
        vll a1(a.begin(), a.begin() + k);
        vll a2(a.begin() + k, a.end());
        vll b1(b.begin(), b.begin() + k);
        vll b2(b.begin() + k, b.end());

        vll a1b1 = karatsubaMultiply(a1, b1);
        vll a2b2 = karatsubaMultiply(a2, b2);

        for (int i = 0; i < k; i++)
            a2[i] += a1[i];
        for (int i = 0; i < k; i++)
            b2[i] += b1[i];

        vll r = karatsubaMultiply(a2, b2);
        for (int i = 0; i < (int) a1b1.size(); i++)
            r[i] -= a1b1[i];
        for (int i = 0; i < (int) a2b2.size(); i++)
            r[i] -= a2b2[i];

        for (int i = 0; i < (int) r.size(); i++)
            res[i + k] += r[i];
        for (int i = 0; i < (int) a1b1.size(); i++)
            res[i] += a1b1[i];
        for (int i = 0; i < (int) a2b2.size(); i++)
            res[i + n] += a2b2[i];
        return res;
    }

    bigint operator*(const bigint &v) const {
        vector<int> a6 = convert_base(this->a, base_digits, 6);
        vector<int> b6 = convert_base(v.a, base_digits, 6);
        vll a(a6.begin(), a6.end());
        vll b(b6.begin(), b6.end());
        while (a.size() < b.size())
            a.push_back(0);
        while (b.size() < a.size())
            b.push_back(0);
        while (a.size() & (a.size() - 1))
            a.push_back(0), b.push_back(0);
        vll c = karatsubaMultiply(a, b);
        bigint res;
        res.sign = sign * v.sign;
        for (int i = 0, carry = 0; i < (int) c.size(); i++) {
            long long cur = c[i] + carry;
            res.a.push_back((int) (cur % 1000000));
            carry = (int) (cur / 1000000);
        }
        res.a = convert_base(res.a, 6, base_digits);
        res.trim();
        return res;
    }
};

bigint add (bigint a , bigint b , bigint c)
{
    return ((a%c)+(b%c))%c;
}

bigint multiply (bigint a , bigint b , bigint c)
{
        return ((a%c)*(b%c))%c;
}

bigint GCD (bigint a , bigint b)
{
    if (a%b==0){return b;}
    else return GCD(b, a%b);
}

bigint modularExpo(bigint a , bigint b , bigint c)
{
         bigint ans=1;
       a=a%c;
    while (b != 0)
    {
        if (b%2 == 1)
        {
            ans = (ans*a)%c;
        }
        a*=a; a=a%c; b/=2;
    }
    return ans;
}

bigint inverse (bigint a , bigint b)
{
    if (gcd(a,b) != 1){return -1;}

    bigint  rm , r  ,rp, x=1 , y , xm=0 , ym=1 , t;
    r = a%b;
    rm = b;
    y= - (a/b);

    while (r != 1){t= x;
    x = xm - (rm/r)*x;
    xm = t;

    t = y;
    y = ym - (rm/r)*y;
    ym=t;

    rp = rm%r;
    rm=r;
    r=rp;
    }

    if(y>=0){return (y%a);}
    else{return (a+ (y%a));}
}

long long int len;
long long int maxl = pow(2,10);

void printArray(bigint arr[], long long int i, long long int j) {
    cout << "[";

    for (long long int start = i; start < j; start++)
        cout << arr[start] << ", ";

    cout << arr[j] << "]";
}


void merge(bigint arr[], long long int i, long long int mid, long long int j) {
    bigint temp[len];
    long long int l = i;
    long long int r = j;
    long long int m = mid + 1;
    long long int k = l;

    while(l <= mid && m <= r) {
        if(arr[l] <= arr[m]) {
            temp[k++] = arr[l++];
        }
        else {
            temp[k++] = arr[m++];
        }
    }

    while(l <= mid)
        temp[k++] = arr[l++];

    while(m <= r) {
        temp[k++] = arr[m++];
    }

    for(long long int i1 = i; i1 <= j; i1++) {
        arr[i1] = temp[i1];
    }


}


void mergesort(bigint arr[], long long int i,long long int j) {
    long long int mid = 0;

    if(i < j) {
        mid = (i + j) / 2;
        mergesort(arr, i, mid);
        mergesort(arr, mid + 1, j);
        merge(arr, i, mid, j);
    }
}

bigint discLog (bigint h , bigint g , bigint p)
{
        long long int m=0;
        while(true){
            bigint k = m*m;
            if(k>p){break;}
            m+=1;
        }

        len=m;

        bigint arr[m] , arr2[m];
        arr2[0]=1;arr[0]=1;

        for (long long int i = 1 ; i< m ; i++)
        {
            arr[i] = multiply(arr[i-1] , g , p);
            arr2[i] = arr[i];
        }

        bigint gminv = multiply (arr[m-1] , g , p); gminv = inverse (p , gminv);

         mergesort(arr, 0, m - 1);

        long long int i;
        for ( i = 0 ; i<m ; i++)
        {

         long long int beg = 0 , endi= m , mid;

mid = (endi +beg)/2;

bigint item;
item = h;

while (beg<=endi && arr[mid]!= item)
{
    if (arr[mid]<item) beg=mid+1;
    else endi=mid-1;
    mid = (beg+endi)/2;
}

if (arr[mid]==item) {break;}

         h = multiply (h , gminv , p);

        }
    long long int j;
    for (j = 0 ; j<m ; j++)
    {
        if (arr2[j] == h){break;}
    }

    return ((i*m) + j);
}

bigint rsaEncrypt (bigint input , bigint e , bigint n)
{
    return modularExpo (input , e , n);
}

bigint rsaDecrypt (bigint input , bigint d , bigint n)
{
        return modularExpo(input , d , n);
}

bool Miller(bigint p,int iteration)
{
    if (p < 2)
    {
        return false;
    }
    if (p != 2 && p % 2==0)
    {
        return false;
    }
    bigint s = p - 1;
    while (s % 2 == 0)
    {
        s /= 2;
    }
    for (int i = 0; i < iteration; i++)
    {
        long long int aa = rand();
        bigint a = aa;
         a = a % (p - 1) + 1;
         bigint temp = s;
        bigint mod = modularExpo(a, temp, p);
        while (temp != p - 1 && mod != 1 && mod != p - 1)
        {
            mod = multiply(mod, mod, p);
            temp *= 2;
        }
        if (mod != p - 1 && temp % 2 == 0)
        {
            return false;
        }
    }
    return true;
}

long long int random ()
{
    int a = 0;
srand((unsigned)time(0));
for(int i=0; i<5; i++){
        a=rand();
}
return a;

}

bigint power (int a)
{
     bigint ans=1 , b=2;

    while (a != 0)
    {
        if (a%2 == 1)
        {
            ans = (ans*b);
        }
        b*=b;  a/=2;
    }
    return ans;
}

void rsaKeyGen (int b , bigint &p , bigint &q , bigint &n, bigint &d , bigint &totientn)   //b = number of bits in b
{
    int bb=b/2;
    p = power(0.9*bb) + pow(random() , 3);
    q = power(1.1*bb) - pow(random() , 3);

    if (p%2 == 0){p+=1;}
    if (q%2 == 0){q+=1;}

    while (! Miller(p , 5))
    {
        p+=2;
    }

    while (! Miller(q , 5))
    {
        q+=2;
    }

    n= p * q;
    totientn = n - p - q + 1;
    bigint e= 65537;
    d = inverse(totientn , e);

    return;
}

void inputPoly1 (long long int &m)
{
     cout<<"Enter the degree of the binary field: ";
    cin>>m;
    return;
}

void inputPoly2 (bool arr[] , long long int m)
{

    for (long long int i = 0 ; i<m ; i++)
    {
        cout<<"Enter the coefficient of x^"<<i<<": ";
        cin>>arr[i];
    }
    return;
}

void printPoly (bool arr[] , long long int m)
{
    for (long long int i = 0 ; i<m ; i++ )
    {
        if (arr[i]== 1) cout<<"+ x^"<<i<<" ";
    }
    cout<<endl;
    return;
}

void addSubPoly (bool arr1[] , bool arr2[] , long long int m1 , long long int m2)
 {

    if (m1 != m2 ){cout<<"The degrees are not the same!"<<endl; return;}

    for(long long int j=0; j<m1; j++){
        if(arr1[j]==arr2[j]){arr1[j]=0;}
        else arr1[j]=1;
    }
    return;
}


void inputPrimeField (bigint arr[] , bigint &p)
    {
        cout<<"Input the prime number of the prime field: ";
        cin>>p;
        cout<<"Keep inputting the power of x and then the coefficient and input -1 as the power to stop: ";
        long long int pow ;
        bigint coef;
        cin>>pow;

        while(pow !=-1)
        {
            cin>>coef;
            arr[pow] = coef;
            cin>>pow;
        }
        return;
    }

void printPrimeField (bigint arr[])
{
    for (int i = 0 ; i<maxl ; i++)
    {
        if (arr[i] != 0){cout<<"+ "<<arr[i]<<"x^"<<i<<" ";}

    }
    return;
}

void addprime (bigint arr1[],bigint arr2[], bigint p1 , bigint p2){
if(p1!=p2){cout<<"Prime fields are incompatible!"<<endl<<endl;}
else{
        for(int i=0; i<maxl ; i++){
            arr1[i] = (arr1[i]+ arr2[i])%p1;

        }


}return;
}


void subprime (bigint arr1[],bigint arr2[], bigint p1 , bigint p2){
if(p1!=p2){cout<<"Prime fields are incompatible!"<<endl<<endl;}
else{
        for(int i=0; i<maxl ; i++){
            arr1[i] = (arr1[i]-arr2[i])%p1;
            if(arr1[i]<0){arr1[i] += p1;}

        }


}return;
}

void multPrime (bigint arr1[], bigint arr2[], bigint p1 , bigint p2){

if(p1 != p2){cout<<"Prime fields are incompatible!"<<endl<<endl;}
else{
        bigint temp[maxl] , mult[maxl];
for(int i=0; i< maxl; i++){
    if(arr2[i]!=0){
        for(int j=0; j<maxl; j++){
            if(arr1[j]!=0){
                temp[i+j]+=(arr2[i]*arr1[j])%p1 ;
                temp[i+j] = temp[i+j]%p1;
            }
        }

    }
}
    for(int k=0; k<maxl; k++){
        arr1[k] = temp[k];
    }
}
}

int main(){
   while (true)
   {float n;
    cout<<"MAIN MENU"<<endl<<endl<<"To compute modular addition multiplication or exponentiation, press 1"<<endl<<"To compute gcd or modular inverse, press 2"<<endl;
    cout<<"To compute discrete logarithm, press 3"<<endl<<"To generate an RSA key, press 4"<<endl<<"To encrypt a message, press 5"<<endl;
    cout<<"To decrypt a message, press 6"<<endl<<"To perform prime field operations, press 7"<<endl;

    cin>>n;
    if (n!=1 && n!=2 && n!=3 && n!=4 && n!=5 && n!=6 && n!=7){cout<<"Invalid Input!!!"<<endl<<endl;}

    else if (n==1)
    {
        int p;
        cout<<endl<<"For modular addition, press 1"<<endl<<"For modular multiplication, press 2"<<endl;
        cout<<"For modular exponentiation, press 3"<<endl;
        cin>>p;

        if(p<1 || p>3){cout<<"invalid input!!!"<<endl;}

        else if (p==1)
        {
            bigint a , b, c;
            cout<<"Please enter number 1: ";
            cin>>a;
            cout<<"Please enter number 2: ";
            cin>>b;
            cout<<"Please enter the order of the group: ";
            cin>>c;
            cout<<"The sum of "<<a<<" and "<<b<<" mod "<<c<<" is: "<<add(a , b , c)<<endl<<endl;
        }

        else if (p==2)
        {
            bigint a , b, c;
            cout<<"Please enter number 1: ";
            cin>>a;
            cout<<"Please enter number 2: ";
            cin>>b;
            cout<<"Please enter the order of the group: ";
            cin>>c;
            cout<<"The product of "<<a<<" and "<<b<<" mod "<<c<<" is: "<<multiply(a , b , c)<<endl<<endl;
        }
        else if(p==3)
        {
            bigint a , b, c;
            cout<<"Please enter number 1: ";
            cin>>a;
            cout<<"Please enter number 2: ";
            cin>>b;
            cout<<"Please enter the order of the group: ";
            cin>>c;
            cout<<"The power of "<<a<<" to the power "<<b<<" mod "<<c<<" is: "<<modularExpo(a , b , c)<<endl<<endl;
        }
    }
    else if(n==2){
        int p;
        cout<<"To find the gcd of two numbers, press 1"<<endl<<"To find the inverse of a number modulo another number, press 2"<<endl;
        cin>>p;

        if(p!=1 && p!=2){cout<<"Invalid Input!"<<endl;}

        else if(p==1){
            bigint a, b;
            cout<< "Enter the first number:"<<endl;
            cin>>a;
            cout<< "Enter the second number:"<<endl;
            cin>>b;
            cout<< "The gcd of "<< a << " and "<< b << " is: " << GCD(a,b)<<endl<<endl;
        }


        else if(p==2){
            bigint a, b;
            cout<< "Enter the number whose inverse is to be found:"<<endl;
            cin>>a;
            cout<< "Enter the number which is the order of the prime field:"<<endl;
            cin>>b;
            cout<< "The inverse of "<< a << " modulo "<< b << " is: " << inverse(b,a)<<endl<<endl;
        }

    }
    else if(n==3){
        bigint a,b,c;
        cout<<"Enter the number whose discrete logarithm is to be found: " <<endl;
        cin>> b;
        cout<< "Enter the base of the logarithm:" << endl;
        cin>>a;
        cout<<"Enter the order of the prime field:" <<endl;
        cin>>c;
        cout << "The discrete logarithm of " << b << " to the base "<< a << " is: " << discLog(b,a,c) <<endl<<endl;
    }

    else if(n==4)
            {
                int b;
                bigint pp,qq,nn,dd,tot;
                cout<<"Please enter the number of bits (approximately) you want in the public key (for a large number of bits, generation of random prime numbers will take a little time) : ";
                cin>>b;
                rsaKeyGen(b , pp , qq,nn,dd,tot);
                cout<<endl<<"Your public key is: "<<endl<<"n = "<<nn<<endl<<"e = "<<65537<<endl;
                cout<<endl<<"Your private key is: d = "<<dd<<endl<<endl;
            }
    else if(n==5)
    {
        bigint input , e=65537, n;
        cout<<" Enter the number that is to be encrypted: " <<endl;
        cin>> input;
        cout<< "Enter your public key: "<< endl;
        cin>>n;
        cout<< "The encrpyted number is: "<< rsaEncrypt(input,e,n)<<endl<<endl;

    }
    else if(n==6)
    {
        bigint input , d , n;
        cout<<" Enter the number that is to be decrypted: " <<endl;
        cin>> input;
        cout<< "Enter your public key: "<< endl;
        cin>>n;
        cout<<"Enter your private key: " <<endl;
        cin>>d;
        cout<< "The decrpyted number is: "<< rsaDecrypt(input,d,n)<<endl<<endl;
    }
   else if (n==7)
   {
       int p;
       cout<<"To add two polynomials in a prime field, press 1"<<endl<<"To subtract one polynomial from another in a prime field, press 2"<<endl<<"To multiply two polynomials in a prime field, press 3"<<endl;
        cout<<"To go back to the main menu, press 0"<<endl;
        cin>>p;

        while (p != 0)
        {
            if (p != 1 && p != 2 && p != 3){cout<<"invalid input!!!"<<endl;}

            else if (p==1)
                     {
                        bigint arr1[maxl] , arr2[maxl] , pp1 , pp2;
                        cout<<"Please enter the first polynomial:"<<endl;
                        inputPrimeField(arr1 , pp1);
                        cout<<"Please enter the second polynomial:"<<endl;
                        inputPrimeField(arr2 , pp2);
                        addprime(arr1 , arr2, pp1, pp2);
                        if (pp1 == pp2){
                        cout<<"The sum is: ";
                        printPrimeField(arr1);
                        cout<<endl<<endl;}
                     }

            else if (p==2)
            {
                        bigint arr1[maxl] , arr2[maxl] , pp1 , pp2;
                        cout<<"Please enter the first polynomial:"<<endl;
                        inputPrimeField(arr1 , pp1);
                        cout<<"Please enter the second polynomial:"<<endl;
                        inputPrimeField(arr2 , pp2);
                        subprime(arr1 , arr2, pp1, pp2);
                        if (pp1==pp2){
                        cout<<"The difference is: ";
                        printPrimeField(arr1);
                        cout<<endl<<endl;}
            }

             else if (p==3)
            {
                        bigint arr1[maxl] , arr2[maxl] , pp1 , pp2;
                        cout<<"Please enter the first polynomial:"<<endl;
                        inputPrimeField(arr1 , pp1);
                        cout<<"Please enter the second polynomial:"<<endl;
                        inputPrimeField(arr2 , pp2);
                        multPrime(arr1 , arr2, pp1, pp2);
                        if (pp1==pp2){
                        cout<<"The product is: ";
                        printPrimeField(arr1);
                        cout<<endl<<endl;}
            }
                    cout<<"To add two polynomials in a prime field, press 1"<<endl<<"To subtract one polynomial from another in a prime field, press 2"<<endl<<"To multiply two polynomials in a prime field, press 3"<<endl;
        cout<<"To go back to the main menu, press 0"<<endl;
        cin>>p;

        }
   }

   }
    return 0;
    }














