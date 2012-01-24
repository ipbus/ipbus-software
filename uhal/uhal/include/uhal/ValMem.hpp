#ifndef _uhal_ValMem_hpp_
#define _uhal_ValMem_hpp_

#include <boost/shared_ptr.hpp>

#include <vector>
#include <iostream>

namespace uhal {  
  class NonValidatedMemory: public std::exception {  };
  class ValMemImutabilityViolation: public std::exception { };
 
  
  class ValWord {
  public:
    ValWord(const uint32_t& value)
      :valid_(new bool(false)),
       value_(new uint32_t(value))
    { 
    };

    ValWord(const ValWord& val)
    :valid_(val.valid_),
     value_(val.value_)
    { 
    };
    
    ValWord()
      :valid_(new bool(false)),
       value_(new uint32_t())
    {
    }
    
    bool valid() {
      return *valid_;
    }
    
    void valid(bool valid) {
      *valid_ = valid;
    }

    ValWord& operator =(const uint32_t& v) {
      value(v);
      
      return *this;
    }

    operator const uint32_t&() {
      return value();
    }
    
    const uint32_t& value() const {
      if (*valid_) 
	return *value_;
      else
	throw NonValidatedMemory();
    }

    void value(const uint32_t& value) {
      if (!*valid_) {
	*value_ = value;
      } else
	throw ValMemImutabilityViolation();
    }
    
  private:
    boost::shared_ptr<bool> valid_;
    boost::shared_ptr<uint32_t> value_;

  };

  class ValBlock {
  public:
    ValBlock(std::vector<uint32_t>& values)
      :valid_(new bool(false)),
       values_(new std::vector<uint32_t>(values))
    { 
    }

    ValBlock(const ValBlock& values)
    :valid_(values.valid_),
     values_(values.values_)
    { 
    }

    ValBlock(uint32_t size)
      :valid_(new bool(false)),
       values_(new std::vector<uint32_t>(size))
    { 
    }
    
    ValBlock()
      :valid_(new bool(false)),
       values_(new std::vector<uint32_t>())
    {
    }
    
    bool valid() {
      return *valid_;
    }
    
    void valid(bool valid) {
      *valid_ = valid;
    }
    
    template <class InputIterator>
    void assign ( InputIterator first, InputIterator last ) {
      if (!*valid_) {
	values_->assign(first,last);	
      } else
	throw ValMemImutabilityViolation();
    }
    
    void push_back ( const uint32_t& v) {
      if (!*valid_) {
	values_->push_back(v);
      } else
	throw ValMemImutabilityViolation();
      
    }

    const uint32_t& operator[] ( uint32_t n ) const {
      if (*valid_) {
	return (*values_)[n];
      } else
	throw NonValidatedMemory();
    
    }

    const uint32_t& at( std::size_t n ) const {
     if (*valid_) {
       return values_->at(n);
      } else
	throw NonValidatedMemory();
    }

    std::size_t size() const {
     if (*valid_) {
       return values_->size();
      } else
       throw NonValidatedMemory();
    }

    void clear() {
      *valid_ = false;
      values_->clear();
    }
    
    std::vector<uint32_t >::const_iterator begin() const {
     if (*valid_) {
       return values_->begin();
      } else
       throw NonValidatedMemory();
     
    }

    std::vector<uint32_t >::const_iterator end() const{
     if (*valid_) {
       return values_->end();
      } else
       throw NonValidatedMemory();
     
    }

    std::vector<uint32_t >::const_reverse_iterator rbegin() {
     if (*valid_) {
       return values_->rbegin();
      } else
       throw NonValidatedMemory();
     
    }
    std::vector<uint32_t >::const_reverse_iterator rend() const {
     if (*valid_) {
       return values_->rend();
      } else
       throw NonValidatedMemory();
     
    }
    

    
  private:
    boost::shared_ptr<bool> valid_;
    boost::shared_ptr<std::vector<uint32_t> > values_;

  };

  
  
      
}

#endif 

