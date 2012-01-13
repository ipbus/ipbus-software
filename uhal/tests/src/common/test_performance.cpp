#include <boost/test/unit_test.hpp>

#include "uhal/uhal.hpp"

#include <string>
#include <cstdlib>


void single_read_performance() {

}
void single_write_performance() {

}
void block_read_performance() {

}
void block_write_performance() {

}
void block_read_non_incremental_performance() {

}
void block_write_non_incremental_performance() {

}

boost::unit_test::test_suite* init_unit_test_suite ( int argc, char* argv[] ) {

  boost::unit_test::test_suite* test = BOOST_TEST_SUITE( "Master test suite" );

  test->add( BOOST_TEST_CASE( &single_read_performance) );
  test->add( BOOST_TEST_CASE( &single_write_performance) );
  test->add( BOOST_TEST_CASE( &block_read_performance) );
  test->add( BOOST_TEST_CASE( &block_write_performance) );
  test->add( BOOST_TEST_CASE( &block_read_non_incremental_performance) );
  test->add( BOOST_TEST_CASE( &block_write_non_incremental_performance) );

  return test;
}
