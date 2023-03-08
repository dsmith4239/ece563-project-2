#include "sim_ooo.h"
#include <iostream>
#include <stdlib.h>
#include <sstream>

using namespace std;

/* Test case for pipelined simuator */ 
/* DO NOT MODIFY */

sim_t sim_ooo;

/* convert a float into an unsigned */
inline unsigned float2unsigned(float value){
        unsigned result;
        memcpy(&result, &value, sizeof value);
        return result;
}

/* convert an unsigned into a float */
inline float unsigned2float(unsigned value){
        float result;
        memcpy(&result, &value, sizeof value);
        return result;
}

int main(int argc, char **argv){

	unsigned i, j;

	// instantiates sim_ooo with a 1MB data memory
	sim_ooo_init(1024*1024, 
				   6,
				   3, 2, 2, 2,
				   4);

	//initialize execution units
        init_exec_unit(INTEGER, 2, 2);
        init_exec_unit(ADDER, 2, 2);
        init_exec_unit(MULTIPLIER, 10, 1);
        init_exec_unit(DIVIDER, 40, 1);
        init_exec_unit(MEMORY, 1, 1);

	//loads program in instruction memory at address 0x00000000
	load_program("asm/code_ooo2.asm", 0x00000000);

	//initialize general purpose registers
	for (i=0; i<5; i++) set_fp_register(i, (float)i);

	//initialize data mem ry and prints its content (for the specified address ranges)
	for (i = 0xA000, j=0; i<0xA020; i+=4, j+=1) write_memory(i,float2unsigned((float)(j+1)));	
	
	cout << "\nBEFORE PROGRAM EXECUTION..." << endl;
	cout << "======================================================================" << endl << endl;
	
	//prints the value of the memory and registers
	print_registers();
	print_memory(0xA000, 0xA020);

	// executes the program	
	cout << "\n*****************************" << endl;
	cout << "STARTING THE PROGRAM..." << endl;
	cout << "*****************************" << endl << endl;

	// first 20 clock cycles
	cout << "First 20 clock cycles: inspecting the registers at each clock cycle..." << endl;
	cout << "======================================================================" << endl << endl;

	for (i=0; i<20; i++){
		cout << "CLOCK CYCLE #" << dec << i << endl;
		run(1);
		print_status();
		cout << endl;
	}

	// runs program to completion
	cout << "EXECUTING PROGRAM TO COMPLETION..." << endl << endl;
	run(); 

	cout << "PROGRAM TERMINATED\n";
	cout << "===================" << endl << endl;

	//prints the value of registers and data memory
	print_status();
	print_memory(0xA000, 0xA020);
	cout << endl;

	//print the execution log
	print_log();
	
	cout << endl;

	// prints the number of instructions executed and IPC
	cout << "Instruction executed = " << dec << get_instructions_executed() << endl;
	cout << "Clock cycles = " << dec << get_clock_cycles() << endl;
	cout << "IPC = " << dec << get_IPC() << endl;
	
	sim_ooo_terminate();
}
