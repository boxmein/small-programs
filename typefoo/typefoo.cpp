#include <iostream>
namespace box {
  class B {
    public:
      double internalx;
      B(double x): internalx(x) {}
  };
  class A {
    public:
      int internal_data;
      // Base constructor
      A(): internal_data(5) {}
      // Constructor called on instantiation
      A(const B& x): internal_data(x.internalx) {}
      
      // Assignment operator
      // B foo;
      // A wow = foo;
      A& operator= (const B& x) {
        internal_data = x.internalx > 0 ? 1 : 2;
        return *this;
      }

      // Type cast
      operator B() {
        return B(internal_data);
      }
  };
}

int main() {
  box::B barf(1.25);
  box::A arf(barf);
  box::A arf2 = barf;
  box::A arf3 = static_cast<box::A>(barf);

  std::cout << "Barf was: " << barf.internalx;
  std::cout << "; arf is " << arf.internal_data;
  std::cout << "; arf2 is " << arf2.internal_data;
  std::cout << "; arf3 is " << arf3.internal_data
            << std::endl;
  
  exit(0);
}
