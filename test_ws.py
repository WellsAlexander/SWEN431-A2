#!/usr/bin/env python3
import os
import subprocess
import glob
import difflib
import sys
from pathlib import Path
import colorama
from colorama import Fore, Style

# Initialize colorama for colored terminal output
colorama.init()

def run_test(input_file, expected_output):
    """
    Run the Haskell program with a specific input file and compare with expected output
    """
    # Extract file number from input file (e.g., "input-001.txt" -> "001")
    file_number = os.path.basename(input_file).split('-')[1].split('.')[0]
    output_file = f"output/output-{file_number}.txt"
    
    # Run the Haskell program
    try:
        subprocess.run(["runghc", "ws.hs", input_file], check=True)
    except subprocess.CalledProcessError as e:
        print(f"{Fore.RED}Error running Haskell program for {input_file}: {e}{Style.RESET_ALL}")
        return False
    
    # Check if output file was created
    if not os.path.exists(output_file):
        print(f"{Fore.RED}Output file {output_file} was not created{Style.RESET_ALL}")
        return False
    
    # Read the output and expected output files
    with open(output_file, 'r') as f:
        actual_output = f.read().strip()
    
    with open(expected_output, 'r') as f:
        expected = f.read().strip()
    
    # Compare the outputs
    if actual_output == expected:
        print(f"{Fore.GREEN}Test passed for {input_file} ✓{Style.RESET_ALL}")
        return True
    else:
        print(f"{Fore.RED}Test failed for {input_file} ✗{Style.RESET_ALL}")
        print(f"{Fore.YELLOW}Differences:{Style.RESET_ALL}")
        
        # Generate diff
        diff = difflib.unified_diff(
            expected.splitlines(),
            actual_output.splitlines(),
            lineterm='',
            fromfile='Expected',
            tofile='Actual'
        )
        
        for line in diff:
            if line.startswith('+'):
                print(f"{Fore.GREEN}{line}{Style.RESET_ALL}")
            elif line.startswith('-'):
                print(f"{Fore.RED}{line}{Style.RESET_ALL}")
            else:
                print(line)
                
        return False

def main():
    # Create directories for input and expected if they don't exist
    Path("input").mkdir(exist_ok=True)
    Path("expected").mkdir(exist_ok=True)
    
    # Find all input files
    input_files = sorted(glob.glob("input/input-*.txt"))
    
    if not input_files:
        print(f"{Fore.YELLOW}No input files found in the input/ directory.{Style.RESET_ALL}")
        return
    
    # Track test results
    passed = 0
    failed = 0
    
    # Set to track which test types were run
    test_types = set()
    
    print(f"{Fore.BLUE}Running tests for Stack Deluxe interpreter...{Style.RESET_ALL}\n")
    
    # Run tests for each input file
    for input_file in input_files:
        # Determine expected output file
        file_number = os.path.basename(input_file).split('-')[1].split('.')[0]
        expected_output = f"expected/expected-{file_number}.txt"
        
        # Track which category of tests we're running (core, completion, challenge)
        test_category = "core" if file_number.startswith("0") else "completion" if file_number.startswith("1") else "challenge"
        test_types.add(test_category)
        
        if os.path.exists(expected_output):
            # Run the test
            if run_test(input_file, expected_output):
                passed += 1
            else:
                failed += 1
        else:
            print(f"{Fore.YELLOW}Expected output file {expected_output} not found{Style.RESET_ALL}")
    
    # Print summary
    print(f"\n{Fore.BLUE}Test Summary:{Style.RESET_ALL}")
    print(f"Categories tested: {', '.join(sorted(test_types))}")
    print(f"Total tests: {passed + failed}")
    print(f"{Fore.GREEN}Passed: {passed}{Style.RESET_ALL}")
    print(f"{Fore.RED}Failed: {failed}{Style.RESET_ALL}")
    
    if failed == 0 and passed > 0:
        print(f"\n{Fore.GREEN}All tests passed! 🎉{Style.RESET_ALL}")
    elif passed == 0:
        print(f"\n{Fore.RED}All tests failed! 😢{Style.RESET_ALL}")
    else:
        print(f"\n{Fore.YELLOW}Some tests failed. Please check the output for details.{Style.RESET_ALL}")

if __name__ == "__main__":
    main()