#!/bin/zsh

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m'

c_npass=0
c_nfail=0
s_npass=0
s_nfail=0

echo "Running concrete test cases:"
for test_file in test/concrete/*.c; do
  if [ -f "$test_file" ]; then
    echo "Running test case: $test_file"
    esy x instantiation exec --logging=disabled "$test_file"
    if [ $? -eq 0 ]; then
      echo "${GREEN}Test case passed: $test_file${NC}"
      ((c_npass++))
    else
      echo "${RED}Test case failed: $test_file${NC}"
      ((c_nfail++))
    fi
  fi
done 

echo "Running symbolic test cases:"
for test_file in test/symbolic/*.c; do
  if [ -f "$test_file" ]; then
    echo "Running test case: $test_file"
    esy x instantiation wpst --logging=disabled "$test_file"
    if [ $? -eq 0 ]; then
      echo "${GREEN}Test case passed: $test_file${NC}"
      ((s_npass++))
    else
      echo "${RED}Test case failed: $test_file${NC}"
      ((s_nfail++))
    fi
  fi
done 

echo "${GREEN}$c_npass concrete tests passed${NC}, ${RED}$c_nfail concrete tests failed${NC}"
echo "${GREEN}$s_npass symbolic tests passed${NC}, ${RED}$s_nfail symbolic tests failed${NC}"
