#include <iostream>
#include <boost/shared_ptr.hpp>

class A {
  public:
  virtual ~A(){};
  virtual void print() {std::cout << "A" << std::endl;};
};
class B: public A {
public:
  virtual void print() {std::cout << "B" << std::endl;};
};

class C {
public:
  C(int x)
  :x_(new int(x))
  {}
  void print() {
    std::cout << "C::x_ =" << *x_ << std::endl;
  }
  boost::shared_ptr<int> x_;
};


void  test_reference_initialization() {
  B b;
  A& a = b;
  a.print();
}

void   test_sharedptr_equallity() {
  boost::shared_ptr<int> aa (new int(31));
  boost::shared_ptr<int> bb (new int(32));
  assert(*aa!=*bb);
  assert(aa.get()!=bb.get());

  aa=bb;
  assert(*aa==*bb);
  assert(aa.get()==bb.get());
}

void   test_sharedptr_counting() {
  int* a = new int(31);
  {
    boost::shared_ptr<int> a1 (a);
  }
  //this lines creates a segfault
  //boost::shared_ptr<int> a2 (a);
}

void test_constpointer() {
  B* b = new B();
  b->print();
  //the lines below does not compile 
  //const B* bb = b;
  //bb->print();
}

void test_copysharedpointer() {
  C c1(32);
  c1.print();
  {
    C c2 = c1;
  }
  *(c1.x_) = 33;
  c1.print();
}

void  test_sizeof_shared_ptr() {
  std::cout << "sizeof(boost::shared_ptr<uint32_t>) = " << sizeof(boost::shared_ptr<uint32_t>) << std::endl;

}

int main() {
  test_reference_initialization();
  test_sharedptr_equallity();
  test_sharedptr_counting();
  test_constpointer();
  test_copysharedpointer();
  test_sizeof_shared_ptr();
}
