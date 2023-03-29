#include "sim_ooo.h"
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <cstring>
#include <string>
#include <iomanip>
#include <map>
#include <vector>

using namespace std;

//used for debugging purposes
static const char *stage_names[NUM_STAGES] = {"ISSUE", "EXE", "WR", "COMMIT"};
static const char *instr_names[NUM_OPCODES] = {"LW", "SW", "ADD", "ADDI", "SUB", "SUBI", "XOR", "AND", "MULT", "DIV", "BEQZ", "BNEZ", "BLTZ", "BGTZ", "BLEZ", "BGEZ", "JUMP", "EOP", "LWS", "SWS", "ADDS", "SUBS", "MULTS", "DIVS"};
static const char *res_station_names[5]={"Int", "Add", "Mult", "Load"};

/* =============================================================

   HELPER FUNCTIONS (misc)

   ============================================================= */


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

/* convert integer into array of unsigned char - little indian */
inline void unsigned2char(unsigned value, unsigned char *buffer){
        buffer[0] = value & 0xFF;
        buffer[1] = (value >> 8) & 0xFF;
        buffer[2] = (value >> 16) & 0xFF;
        buffer[3] = (value >> 24) & 0xFF;
}

/* convert array of char into integer - little indian */
inline unsigned char2unsigned(unsigned char *buffer){
       return buffer[0] + (buffer[1] << 8) + (buffer[2] << 16) + (buffer[3] << 24);
}

/* the following six functions return the kind of the considered opcdoe */

bool is_branch(opcode_t opcode){
        return (opcode == BEQZ || opcode == BNEZ || opcode == BLTZ || opcode == BLEZ || opcode == BGTZ || opcode == BGEZ || opcode == JUMP);
}

bool is_memory(opcode_t opcode){
        return (opcode == LW || opcode == SW || opcode == LWS || opcode == SWS);
}

bool is_int_r(opcode_t opcode){
        return (opcode == ADD || opcode == SUB || opcode == XOR || opcode == AND);
}

bool is_int_imm(opcode_t opcode){
        return (opcode == ADDI || opcode == SUBI );
}

bool is_int(opcode_t opcode){
        return (is_int_r(opcode) || is_int_imm(opcode));
}

bool is_fp_alu(opcode_t opcode){
        return (opcode == ADDS || opcode == SUBS || opcode == MULTS || opcode == DIVS);
}

/* clears a ROB entry */
void clean_rob(rob_entry_t *entry){
        entry->ready=false;
        entry->pc=UNDEFINED;
        entry->state=ISSUE;
        entry->destination=UNDEFINED;
        entry->value=UNDEFINED;
		entry->branch_taken = false;
}

/* clears a reservation station */
void clean_res_station(res_station_entry_t *entry){
        entry->pc=UNDEFINED;
        entry->value1=UNDEFINED;
        entry->value2=UNDEFINED;
        entry->tag1=UNDEFINED;
        entry->tag2=UNDEFINED;
        entry->destination=UNDEFINED;
        entry->address=UNDEFINED;
}

/* clears an entry if the instruction window */
void clean_instr_window(instr_window_entry_t *entry){
        entry->pc=UNDEFINED;
        entry->issue=UNDEFINED;
        entry->exe=UNDEFINED;
        entry->wr=UNDEFINED;
        entry->commit=UNDEFINED;
}

/* implements the ALU operation 
   NOTE: this function does not cover LOADS and STORES!
*/
unsigned alu(opcode_t opcode, unsigned value1, unsigned value2, unsigned immediate, unsigned pc){
	unsigned result;
	switch(opcode){
			case ADD:
			case ADDI:
				result = value1+value2;
				break;
			case SUB:
			case SUBI:
				result = value1-value2;
				break;
			case XOR:
				result = value1 ^ value2;
				break;
			case AND:
				result = value1 & value2;
				break;
			case MULT:
				result = value1 * value2;
				break;
			case DIV:
				result = value1 / value2;
				break;
			case ADDS:
				result = float2unsigned(unsigned2float(value1) + unsigned2float(value2));
				break;
			case SUBS:
				result = float2unsigned(unsigned2float(value1) - unsigned2float(value2));
				break;
			case MULTS:
				result = float2unsigned(unsigned2float(value1) * unsigned2float(value2));
				break;
			case DIVS:
				result = float2unsigned(unsigned2float(value1) / unsigned2float(value2));
				break;
			case JUMP:
				result = pc + 4 + immediate;
				break;
			default: //branches
				int reg = (int) value1;
				bool condition = ((opcode == BEQZ && reg==0) ||
				(opcode == BNEZ && reg!=0) ||
  				(opcode == BGEZ && reg>=0) ||
  				(opcode == BLEZ && reg<=0) ||      
  				(opcode == BGTZ && reg>0) ||       
  				(opcode == BLTZ && reg<0));
				if (condition)
	 				result = pc+4+immediate;
					//result = 1;
				else 
					result = pc+4;
					//result = 0;
				break;
	}
	return 	result;
}

/* writes the data memory at the specified address */
void sim_ooo::write_memory(unsigned address, unsigned value){
	unsigned2char(value,data_memory+address);
}

/* =============================================================

   Handling of FUNCTIONAL UNITS

   ============================================================= */

/* initializes an execution unit */
void sim_ooo::init_exec_unit(exe_unit_t exec_unit, unsigned latency, unsigned instances){
        for (unsigned i=0; i<instances; i++){
                exec_units[num_units].type = exec_unit;
                exec_units[num_units].latency = latency;
                exec_units[num_units].busy = 0;
                exec_units[num_units].pc = UNDEFINED;
				exec_units[num_units].result = UNDEFINED;
				exec_units[num_units].released_this_cycle = false;
                num_units++;
        }
}

/* returns a free unit for that particular operation or UNDEFINED if no unit is currently available */
unsigned sim_ooo::get_free_unit(opcode_t opcode){
	if (num_units == 0){
		cout << "ERROR:: simulator does not have any execution units!\n";
		exit(-1);
	}
	for (unsigned u=0; u<num_units; u++){
		switch(opcode){
			//Integer unit
			case ADD:
			case ADDI:
			case SUB:
			case SUBI:
			case XOR:
			case AND:
			case BEQZ:
			case BNEZ:
			case BLTZ:
			case BGTZ:
			case BLEZ:
			case BGEZ:
			case JUMP:
				if (exec_units[u].type==INTEGER && exec_units[u].busy==0 && exec_units[u].pc==UNDEFINED) return u;
				break;
			//memory unit
			case LW:
			case SW:
			case LWS: 
			case SWS:
				if (exec_units[u].type==MEMORY && exec_units[u].busy==0 && exec_units[u].pc==UNDEFINED) return u;
				break;
			// FP adder
			case ADDS:
			case SUBS:
				if (exec_units[u].type==ADDER && exec_units[u].busy==0 && exec_units[u].pc==UNDEFINED) return u;
				break;
			// Multiplier
			case MULT:
			case MULTS:
				if (exec_units[u].type==MULTIPLIER && exec_units[u].busy==0 && exec_units[u].pc==UNDEFINED) return u;
				break;
			// Divider
			case DIV:
			case DIVS:
				if (exec_units[u].type==DIVIDER && exec_units[u].busy==0 && exec_units[u].pc==UNDEFINED) return u;
				break;
			default:
				cout << "ERROR:: operations not requiring exec unit!\n";
				exit(-1);
		}
	}
	return UNDEFINED;
}



/* ============================================================================

   Primitives used to print out the state of each component of the processor:
   	- registers
	- data memory
	- instruction window
        - reservation stations and load buffers
        - (cycle-by-cycle) execution log
	- execution statistics (CPI, # instructions executed, # clock cycles) 

   =========================================================================== */
 

/* prints the content of the data memory */
void sim_ooo::print_memory(unsigned start_address, unsigned end_address){
	cout << "DATA MEMORY[0x" << hex << setw(8) << setfill('0') << start_address << ":0x" << hex << setw(8) << setfill('0') <<  end_address << "]" << endl;
	for (unsigned i=start_address; i<end_address; i++){
		if (i%4 == 0) cout << "0x" << hex << setw(8) << setfill('0') << i << ": "; 
		cout << hex << setw(2) << setfill('0') << int(data_memory[i]) << " ";
		if (i%4 == 3){
			cout << endl;
		}
	} 
}

/* prints the value of the registers */
void sim_ooo::print_registers(){
        unsigned i;
	cout << "GENERAL PURPOSE REGISTERS" << endl;
	cout << setfill(' ') << setw(8) << "Register" << setw(22) << "Value" << setw(5) << "ROB" << endl;
        for (i=0; i< NUM_GP_REGISTERS; i++){
                if (get_int_register_tag(i)!=UNDEFINED) 
			cout << setfill(' ') << setw(7) << "R" << dec << i << setw(22) << "-" << setw(5) << get_int_register_tag(i) << endl;
                else if (get_int_register(i)!=(int)UNDEFINED) 
			cout << setfill(' ') << setw(7) << "R" << dec << i << setw(11) << get_int_register(i) << hex << "/0x" << setw(8) << setfill('0') << get_int_register(i) << setfill(' ') << setw(5) << "-" << endl;
        }
	for (i=0; i< NUM_GP_REGISTERS; i++){
                if (get_fp_register_tag(i)!=UNDEFINED) 
			cout << setfill(' ') << setw(7) << "F" << dec << i << setw(22) << "-" << setw(5) << get_fp_register_tag(i) << endl;
                else if (get_fp_register(i)!=UNDEFINED) 
			cout << setfill(' ') << setw(7) << "F" << dec << i << setw(11) << get_fp_register(i) << hex << "/0x" << setw(8) << setfill('0') << float2unsigned(get_fp_register(i)) << setfill(' ') << setw(5) << "-" << endl;
	}
	cout << endl;
}

/* prints the content of the ROB */
void sim_ooo::print_rob(){
	cout << "REORDER BUFFER" << endl;
	cout << setfill(' ') << setw(5) << "Entry" << setw(6) << "Busy" << setw(7) << "Ready" << setw(12) << "PC" << setw(10) << "State" << setw(6) << "Dest" << setw(12) << "Value" << endl;
	for(unsigned i=0; i< rob.num_entries;i++){
		rob_entry_t entry = rob.entries[i];
		instruction_t instruction;
		if (entry.pc != UNDEFINED) instruction = instr_memory[(entry.pc-instr_base_address)>>2]; 
		cout << setfill(' ');
		cout << setw(5) << i;
		cout << setw(6);
		if (entry.pc==UNDEFINED) cout << "no"; else cout << "yes";
		cout << setw(7);
		if (entry.ready) cout << "yes"; else cout << "no";	
		if (entry.pc!= UNDEFINED ) cout << "  0x" << hex << setfill('0') << setw(8) << entry.pc;
		else	cout << setw(12) << "-";
		cout << setfill(' ') << setw(10);
		if (entry.pc==UNDEFINED) cout << "-";		
		else cout << stage_names[entry.state];
		if (entry.destination==UNDEFINED) cout << setw(6) << "-";
		else{
			if (instruction.opcode == SW || instruction.opcode == SWS)
				cout << setw(6) << dec << entry.destination; 
			else if (entry.destination < NUM_GP_REGISTERS)
				cout << setw(5) << "R" << dec << entry.destination;
			else
				cout << setw(5) << "F" << dec << entry.destination-NUM_GP_REGISTERS;
		}
		if (entry.value!=UNDEFINED) cout << "  0x" << hex << setw(8) << setfill('0') << entry.value << endl;	
		else cout << setw(12) << setfill(' ') << "-" << endl;
	}
	cout << endl;
}

/* prints the content of the reservation stations */
void sim_ooo::print_reservation_stations(){
	cout << "RESERVATION STATIONS" << endl;
	cout  << setfill(' ');
	cout << setw(7) << "Name" << setw(6) << "Busy" << setw(12) << "PC" << setw(12) << "Vj" << setw(12) << "Vk" << setw(6) << "Qj" << setw(6) << "Qk" << setw(6) << "Dest" << setw(12) << "Address" << endl; 
	for(unsigned i=0; i< reservation_stations.num_entries;i++){
		res_station_entry_t entry = reservation_stations.entries[i];
	 	cout  << setfill(' ');
		cout << setw(6); 
		cout << res_station_names[entry.type];
		cout << entry.name + 1;
		cout << setw(6);
		if (entry.pc==UNDEFINED) cout << "no"; else cout << "yes";
		if (entry.pc!= UNDEFINED ) cout << setw(4) << "  0x" << hex << setfill('0') << setw(8) << entry.pc;
		else	cout << setfill(' ') << setw(12) <<  "-";			
		if (entry.value1!= UNDEFINED ) cout << "  0x" << setfill('0') << setw(8) << hex << entry.value1;
		else	cout << setfill(' ') << setw(12) << "-";			
		if (entry.value2!= UNDEFINED ) cout << "  0x" << setfill('0') << setw(8) << hex << entry.value2;
		else	cout << setfill(' ') << setw(12) << "-";			
		cout << setfill(' ');
		cout <<setw(6);
		if (entry.tag1!= UNDEFINED ) cout << dec << entry.tag1;
		else	cout << "-";			
		cout <<setw(6);
		if (entry.tag2!= UNDEFINED ) cout << dec << entry.tag2;
		else	cout << "-";			
		cout <<setw(6);
		if (entry.destination!= UNDEFINED ) cout << dec << entry.destination;
		else	cout << "-";			
		if (entry.address != UNDEFINED ) cout <<setw(4) << "  0x" << setfill('0') << setw(8) << hex << entry.address;
		else	cout << setfill(' ') << setw(12) <<  "-";			
		cout << endl;	
	}
	cout << endl;
}

/* prints the state of the pending instructions */
void sim_ooo::print_pending_instructions(){
	cout << "PENDING INSTRUCTIONS STATUS" << endl;
	cout << setfill(' ');
	cout << setw(10) << "PC" << setw(7) << "Issue" << setw(7) << "Exe" << setw(7) << "WR" << setw(7) << "Commit";
	cout << endl;
	for(unsigned i=0; i< pending_instructions.num_entries;i++){
		instr_window_entry_t entry = pending_instructions.entries[i];
		if (entry.pc!= UNDEFINED ) cout << "0x" << setfill('0') << setw(8) << hex << entry.pc;
		else	cout << setfill(' ') << setw(10)  << "-";
		cout << setfill(' ');
		cout << setw(7);			
		if (entry.issue!= UNDEFINED ) cout << dec << entry.issue;
		else	cout << "-";			
		cout << setw(7);			
		if (entry.exe!= UNDEFINED ) cout << dec << entry.exe;
		else	cout << "-";			
		cout << setw(7);			
		if (entry.wr!= UNDEFINED ) cout << dec << entry.wr;
		else	cout << "-";			
		cout << setw(7);			
		if (entry.commit!= UNDEFINED ) cout << dec << entry.commit;
		else	cout << "-";
		cout << endl;			
	}
	cout << endl;
}


/* initializes the execution log */
void sim_ooo::init_log(){
	log << "EXECUTION LOG" << endl;
	log << setfill(' ');
	log << setw(10) << "PC" << setw(7) << "Issue" << setw(7) << "Exe" << setw(7) << "WR" << setw(7) << "Commit";
	log << endl;
}

/* adds an instruction to the log */
void sim_ooo::commit_to_log(instr_window_entry_t entry){
                if (entry.pc!= UNDEFINED ) log << "0x" << setfill('0') << setw(8) << hex << entry.pc;
                else    log << setfill(' ') << setw(10)  << "-";
                log << setfill(' ');
                log << setw(7);
                if (entry.issue!= UNDEFINED ) log << dec << entry.issue;
                else    log << "-";
                log << setw(7);
                if (entry.exe!= UNDEFINED ) log << dec << entry.exe;
                else    log << "-";
                log << setw(7);
                if (entry.wr!= UNDEFINED ) log << dec << entry.wr;
                else    log << "-";
                log << setw(7);
                if (entry.commit!= UNDEFINED ) log << dec << entry.commit;
                else    log << "-";
                log << endl;
}

/* prints the content of the log */
void sim_ooo::print_log(){
	cout << log.str();
}

/* prints the state of the pending instruction, the content of the ROB, the content of the reservation stations and of the registers */
void sim_ooo::print_status(){
	print_pending_instructions();
	print_rob();
	print_reservation_stations();
	print_registers();
}

/* execution statistics */

float sim_ooo::get_IPC(){return (float)instructions_executed/clock_cycles;}

unsigned sim_ooo::get_instructions_executed(){return instructions_executed;}

unsigned sim_ooo::get_clock_cycles(){return clock_cycles;}



/* ============================================================================

   PARSER

   =========================================================================== */


void sim_ooo::load_program(const char *filename, unsigned base_address){

   /* initializing the base instruction address */
   instr_base_address = base_address;

   /* creating a map with the valid opcodes and with the valid labels */
   map<string, opcode_t> opcodes; //for opcodes
   map<string, unsigned> labels;  //for branches
   for (int i=0; i<NUM_OPCODES; i++)
	 opcodes[string(instr_names[i])]=(opcode_t)i;

   /* opening the assembly file */
   ifstream fin(filename, ios::in | ios::binary);
   if (!fin.is_open()) {
      cerr << "error: open file " << filename << " failed!" << endl;
      exit(-1);
   }

   /* parsing the assembly file line by line */
   string line;
   unsigned instruction_nr = 0;
   while (getline(fin,line)){
	
	// set the instruction field
	char *str = const_cast<char*>(line.c_str());

  	// tokenize the instruction
	char *token = strtok (str," \t");
	map<string, opcode_t>::iterator search = opcodes.find(token);
        if (search == opcodes.end()){
		// this is a label for a branch - extract it and save it in the labels map
		string label = string(token).substr(0, string(token).length() - 1);
		labels[label]=instruction_nr;
		// move to next token, which must be the instruction opcode
		token = strtok (NULL, " \t");
		search = opcodes.find(token);
		if (search == opcodes.end()) cout << "ERROR: invalid opcode: " << token << " !" << endl;
	}

	instr_memory[instruction_nr].opcode = search->second;

	//reading remaining parameters
	char *par1;
	char *par2;
	char *par3;
	switch(instr_memory[instruction_nr].opcode){
		case ADD:
		case SUB:
		case XOR:
		case AND:
		case MULT:
		case DIV:
		case ADDS:
		case SUBS:
		case MULTS:
		case DIVS:
			par1 = strtok (NULL, " \t");
			par2 = strtok (NULL, " \t");
			par3 = strtok (NULL, " \t");
			instr_memory[instruction_nr].dest = atoi(strtok(par1, "RF"));
			instr_memory[instruction_nr].src1 = atoi(strtok(par2, "RF"));
			instr_memory[instruction_nr].src2 = atoi(strtok(par3, "RF"));
			break;
		case ADDI:
		case SUBI:
			par1 = strtok (NULL, " \t");
			par2 = strtok (NULL, " \t");
			par3 = strtok (NULL, " \t");
			instr_memory[instruction_nr].dest = atoi(strtok(par1, "R"));
			instr_memory[instruction_nr].src1 = atoi(strtok(par2, "R"));
			instr_memory[instruction_nr].immediate = strtoul (par3, NULL, 0); 
			break;
		case LW:
		case LWS:
			par1 = strtok (NULL, " \t");
			par2 = strtok (NULL, " \t");
			instr_memory[instruction_nr].dest = atoi(strtok(par1, "RF"));
			instr_memory[instruction_nr].immediate = strtoul(strtok(par2, "()"), NULL, 0);
			instr_memory[instruction_nr].src1 = atoi(strtok(NULL, "R"));
			break;
		case SW:
		case SWS:
			par1 = strtok (NULL, " \t");
			par2 = strtok (NULL, " \t");
			instr_memory[instruction_nr].src1 = atoi(strtok(par1, "RF"));
			instr_memory[instruction_nr].immediate = strtoul(strtok(par2, "()"), NULL, 0);
			instr_memory[instruction_nr].src2 = atoi(strtok(NULL, "R"));
			break;
		case BEQZ:
		case BNEZ:
		case BLTZ:
		case BGTZ:
		case BLEZ:
		case BGEZ:
			par1 = strtok (NULL, " \r");
			par2 = strtok (NULL, " \r");	// changed \t to \r (-:
			instr_memory[instruction_nr].src1 = atoi(strtok(par1, "R"));
			instr_memory[instruction_nr].label = par2;
			break;
		case JUMP:
			par2 = strtok (NULL, " \t");
			instr_memory[instruction_nr].label = par2;
		default:
			break;

	} 

	/* increment instruction number before moving to next line */
	instruction_nr++;
   }
   //reconstructing the labels of the branch operations
   int i = 0;
   while(true){
   	instruction_t instr = instr_memory[i];
	if (instr.opcode == EOP) break;
	if (instr.opcode == BLTZ || instr.opcode == BNEZ ||
            instr.opcode == BGTZ || instr.opcode == BEQZ ||
            instr.opcode == BGEZ || instr.opcode == BLEZ ||
            instr.opcode == JUMP
	 ){
		instr_memory[i].immediate = (labels[instr.label] - i - 1) << 2; // had to change provided function
	}
        i++;
   }

}

/* ============================================================================

   Simulator creation, initialization and deallocation 

   =========================================================================== */

sim_ooo::sim_ooo(unsigned mem_size,
                unsigned rob_size,
                unsigned num_int_res_stations,
                unsigned num_add_res_stations,
                unsigned num_mul_res_stations,
                unsigned num_load_res_stations,
		unsigned max_issue){
	//memory
	data_memory_size = mem_size;
	data_memory = new unsigned char[data_memory_size];

	//issue width
	issue_width = max_issue;

	//rob, instruction window, reservation stations
	rob.num_entries=rob_size;
	pending_instructions.num_entries=rob_size;
	reservation_stations.num_entries= num_int_res_stations+num_load_res_stations+num_add_res_stations+num_mul_res_stations;
	rob.entries = new rob_entry_t[rob_size];
	pending_instructions.entries = new instr_window_entry_t[rob_size];
	reservation_stations.entries = new res_station_entry_t[reservation_stations.num_entries];
	unsigned n=0;
	for (unsigned i=0; i<num_int_res_stations; i++,n++){
		reservation_stations.entries[n].type=INTEGER_RS;
		reservation_stations.entries[n].name=i;
	}
	for (unsigned i=0; i<num_load_res_stations; i++,n++){
		reservation_stations.entries[n].type=LOAD_B;
		reservation_stations.entries[n].name=i;
	}
	for (unsigned i=0; i<num_add_res_stations; i++,n++){
		reservation_stations.entries[n].type=ADD_RS;
		reservation_stations.entries[n].name=i;
	}
	for (unsigned i=0; i<num_mul_res_stations; i++,n++){
		reservation_stations.entries[n].type=MULT_RS;
		reservation_stations.entries[n].name=i;
	}
	//execution units
	num_units = 0;
	reset();
}
	
sim_ooo::~sim_ooo(){
	delete [] data_memory;
	delete [] rob.entries;
	delete [] pending_instructions.entries;
	delete [] reservation_stations.entries;
}

/* =============================================================

   CODE TO BE COMPLETED

   ============================================================= */

/* core of the simulator */
void sim_ooo::run(unsigned cycles){	// cycles = stop target
	bool end_of_program = false;
	int local_cycles = -1; // cycles this function call
		pc = real_pc * 4 + instr_base_address;
		bool to_completion = (cycles == 0);
		bool branched_this_cycle = false;
	// if cycles == 0, run until EOP (return)
	// if cycles != 0, run until cycle count 
	while(local_cycles < (int)cycles - 1 || to_completion){
		clock_cycles++;
		local_cycles++;



		


		// Decrement busy execution units
		for(int i = 0; i < num_units; i++){
			if(exec_units[i].busy == 0) exec_units[i].pc = UNDEFINED; // clear last cycle's finished units
			if(exec_units[i].busy > 0) exec_units[i].busy--;	
			if(exec_units[i].released_this_cycle) exec_units[i].released_this_cycle = false;
		}

		for(int i = 0; i < reservation_stations.num_entries; i++){
			reservation_stations.entries[i].received_tag_this_cycle = false;
			reservation_stations.entries[i].instr_exed_this_cycle = false;
		}
		

		// ----------------------------- COMMIT ---------------------------- 
			// Action at top instruction of ROB (head points at this inst.)
			rob_entry_t next_entry = rob.entries[ROB_headptr];
			// If empty (entry.pc == undefined), do nothing. Else:
			if(!(next_entry.pc == UNDEFINED) && next_entry.ready == true){
				instructions_executed++;
			
				instruction_t entry_instruction = instr_memory[(next_entry.pc - instr_base_address) / 4]; // might need to change this if seg fault from 4/1 pc conversion
				pending_instructions.entries[entry_instruction.pending_index].commit = clock_cycles;
			// 	If we are committing last instruction, return at the end of this commit.
				if(next_entry.pc == last_instruction_pc) end_of_program = true;
			// 	If ALU/store instruction and result ready:
				if(next_entry.ready == true && isALUorSTORE(entry_instruction)){
			// 		store in reg/memory, clean this ROB entry, increment head
					// use sets (dest = value)
					clean_rob(&rob.entries[ROB_headptr]);
					if(is_fp_alu(entry_instruction.opcode)){
						unsigned dest = next_entry.destination;
						if(dest >= 32) dest -= 32;
						set_fp_register(dest,unsigned2float(next_entry.value));
					}
					else if(is_int(entry_instruction.opcode)){
						set_int_register(next_entry.destination,unsigned2float(next_entry.value));
					}
					//reset_pending_instruction(0);
				}
				if(next_entry.ready == true && (entry_instruction.opcode == LWS || entry_instruction.opcode == LW)){
					int_fp_registers[next_entry.destination] = unsigned2float(next_entry.value);
					clean_rob(&rob.entries[ROB_headptr]);
					clean_rob(&rob.entries[entry_instruction.rob_index]);
					// write here? .out says F1 is written in wr but what do i know
					//reset_pending_instruction(0);
				}
				// ----------------------------------------------------------
			// 	If branch ( will need adjusting, next pc is computed within branch exe stage)
			//  so exe unit result holds next pc. compare to what would be pc to determine:
				// correct prediction (predict branch is not taken):
				if(next_entry.ready == true && is_branch(entry_instruction.opcode)){
					unsigned target_pc = alu(entry_instruction.opcode,entry_instruction.src1,entry_instruction.src2,entry_instruction.immediate,pc);
			// 		clean this ROB entry, increment head
					if(!next_entry.branch_taken){//if(next_entry.value == 0){ // branch not taken
						clean_rob(&rob.entries[ROB_headptr]);
						//reset_pending_instruction(entry_instruction.pending_index);
					}
			// 	If branch with incorrect prediction / misprediction:
					else{ //if(next_entry.value == 1){ // branch taken
			// 		Clear entire ROB (and execution units?), set PC to correct target address.
						commit_to_log(pending_instructions.entries[entry_instruction.pending_index]);
						int dummy_index = next_entry.pc + 4;
						
						for(int i = 0; i < pending_instructions.num_entries; i++){
							for(int j = 0; j < pending_instructions.num_entries; j++){
								// if this instruction is equal to the committed +4, commit and add another 4
								if(pending_instructions.entries[j].pc == dummy_index) {commit_to_log(pending_instructions.entries[j]);
									reset_pending_instruction(j);
									dummy_index = dummy_index + 4;
								}
							}
						}
						
						for(int i = 0; i < MAX_UNITS; i++){
							exec_units[i].busy = 0;
							exec_units[i].pc = UNDEFINED;
							exec_units[i].released_this_cycle = false;
							exec_units[i].result = UNDEFINED;
						}
						for(int i = 0; i < rob.num_entries; i++) clean_rob(&rob.entries[i]);
						for(int i = 0; i < pending_instructions.num_entries; i++) reset_pending_instruction(i);
						for(int i = 0; i < reservation_stations.num_entries; i++) reset_reservation_station(i);
						//real_pc = rob.entries[ROB_headptr].value;
						//pc = real_pc * 4 + instr_base_address;
						pc = next_entry.value;
						branched_this_cycle = true;
						real_pc = (pc - instr_base_address) / 4;
						ROB_headptr = -1;
						PI_headptr = 0;
						ROB_nextindex = 0;
					}
				}
				// ----------------------------------------------------------
			//	Increment head pointer. If pointer == rob size, wrap back to zero. clear pending instruction
				ROB_headptr++;
				if(ROB_headptr == rob.num_entries) ROB_headptr = 0;
				// COMMIT AND CLEAR PENDING INSTRUCTION
				if(!branched_this_cycle)commit_to_log(pending_instructions.entries[entry_instruction.pending_index]);
				reset_pending_instruction(entry_instruction.pending_index);
				pending_instructions.entries[entry_instruction.pending_index].released_this_cycle = true;

			}

			if(end_of_program) {
				clock_cycles++;
				return;
			}

		// --------------------------- END COMMIT -------------------------- 

	//	unsigned wait_to_write = UNDEFINED;

		// ------------------------------- WR ------------------------------ 
			// For each unit:
			for(int i = 0; i < num_units; i++){
				if(exec_units[i].busy == 0 && exec_units[i].pc != UNDEFINED){
			//  if instruction is done:
			// 		write result to ROB. mark ready & write WR cycle in PI	
					//if(exec_units[i].type == MEMORY || exec_units[i].type == ADDER) {
						exec_units[i].released_this_cycle = true;
					//}
					for(int j = 0; j < rob.num_entries; j++){
						if(rob.entries[j].pc == exec_units[i].pc && rob.entries[j].pc != UNDEFINED){
							rob.entries[j].value = exec_units[i].result;
							rob.entries[j].ready = true;
							rob.entries[j].state = WRITE_RESULT;
							if(pending_instructions.entries[instr_memory[(rob.entries[j].pc - instr_base_address) / 4].pending_index].wr == UNDEFINED) pending_instructions.entries[instr_memory[(rob.entries[j].pc - instr_base_address) / 4].pending_index].wr = clock_cycles;	// 4/1 offset
							// which entry in pending instructions?
							// instr_memory[rob.entries[i].pc].pending_index
							// rob.entries[i].pc pointer to instruction in memory'
							// pending_index stored to instr_memory 

							// clear res station if its pc matches rob entry's pc
							for(int k = 0; k < reservation_stations.num_entries; k++){
								if(reservation_stations.entries[k].pc == rob.entries[j].pc) {
									clean_res_station(&reservation_stations.entries[k]);
									reservation_stations.entries[k].instr_exed_this_cycle = true; // testing
								}
							}
						}
					}
				}
			}
		// ----------------------------- END WR ---------------------------- 

		
		// ------------------------------ EXE ------------------------------ 
			// might exe twice? might need to check pending_instructions.entries[instr_memory[(reservation_stations.entries[j].pc - instr_base_address) / 4].pending_index].exe
			
			
			// For each RS:
			for(int j = 0; j < reservation_stations.num_entries; j++){
				instruction_t entry_instruction;
			//  If station is waiting on operands, monitor for broadcast. otherwise,
				if(reservation_stations.entries[j].pc != UNDEFINED && reservation_stations.entries[j].received_tag_this_cycle == false) entry_instruction = instr_memory[(reservation_stations.entries[j].pc - instr_base_address) / 4]; // 4/1
				if((
			
						// load rs: needs Vj
						(entry_instruction.opcode == LWS || entry_instruction.opcode == LW) && (
							1
						)
						// int/fp alu rs: needs Vj Vk
						|| (is_fp_alu(entry_instruction.opcode) || is_int(entry_instruction.opcode)) && (
							reservation_stations.entries[j].value1 != UNDEFINED && reservation_stations.entries[j].value2 != UNDEFINED
						) || (is_branch(entry_instruction.opcode) && reservation_stations.entries[j].value1 != UNDEFINED)
						
					)
					//reservation_stations.entries[j].tag1 != UNDEFINED && reservation_stations.entries[j].tag2 != UNDEFINED
						&& reservation_stations.entries[j].pc != UNDEFINED
				){
			//		if unit is available and has operands,
					
					unsigned unit_num = get_free_unit(entry_instruction.opcode);
					if(unit_num != UNDEFINED && exec_units[unit_num].released_this_cycle == false){
						// send to unit, mark as busy, compute result
						reservation_stations.entries[j].instr_exed_this_cycle = true;	// update rs so a new instruction doesnt enter same cycle
						exec_units[unit_num].busy = exec_units[get_free_unit(entry_instruction.opcode)].latency;
						exec_units[unit_num].pc = reservation_stations.entries[j].pc;
						if(is_fp_alu(entry_instruction.opcode) || is_int(entry_instruction.opcode) || is_branch(entry_instruction.opcode)) {
							exec_units[unit_num].result = alu(entry_instruction.opcode, reservation_stations.entries[j].value1, reservation_stations.entries[j].value2, entry_instruction.immediate, reservation_stations.entries[j].pc);
							if(exec_units[unit_num].result == pc+entry_instruction.immediate){ // branch taken
								rob.entries[instr_memory[(reservation_stations.entries[j].pc - instr_base_address) / 4].rob_index].branch_taken = true;
							}
						}
						if(
							exec_units[unit_num].type==MEMORY && (
								entry_instruction.opcode == LWS || 
								entry_instruction.opcode == LW
								)){
									unsigned base_byte_index = entry_instruction.immediate + get_int_register(entry_instruction.src1);
									unsigned lmd = UNDEFINED;
									
									lmd = data_memory[base_byte_index] + (data_memory[base_byte_index + 1] << 8) + (data_memory[base_byte_index + 2] << 16) + (data_memory[base_byte_index + 3] << 24);
									exec_units[unit_num].result = lmd;
									reservation_stations.entries[j].address = base_byte_index;
									
								}
						if(pending_instructions.entries[instr_memory[(reservation_stations.entries[j].pc - instr_base_address) / 4].pending_index].exe == UNDEFINED){
							pending_instructions.entries[instr_memory[(reservation_stations.entries[j].pc - instr_base_address) / 4].pending_index].exe = clock_cycles;
						}
						//if(is_branch(entry_instruction.opcode)) 
						// set state to exe in rob
						rob.entries[instr_memory[(reservation_stations.entries[j].pc - instr_base_address) / 4].rob_index].state = EXECUTE;
					}
				}	
			}
		// ---------------------------- END EXE ---------------------------- 


		// CDB broadcast (moved from after WR to after EXE)

		// 		Broadcast result to all reservation stations in case they
		// 		were waiting on tag from this execution unit. 
		// 			(search all RS for a matching tag, update if tag matches the unit's instruction's entry on the rob's index)
			

		for(int i = 0; i < num_units; i++){
			if(exec_units[i].busy == 0){
				for(int j = 0; j < rob.num_entries; j++){
					for(int k = 0; k < reservation_stations.num_entries; k++){
						if(reservation_stations.entries[k].tag1 == instr_memory[(exec_units[i].pc - instr_base_address) / 4].rob_index){
							reservation_stations.entries[k].value1 = exec_units[i].result;
							reservation_stations.entries[k].tag1 = UNDEFINED;
							reservation_stations.entries[k].received_tag_this_cycle = true;
						}
						if(reservation_stations.entries[k].tag2 == instr_memory[(exec_units[i].pc - instr_base_address) / 4].rob_index){
							reservation_stations.entries[k].value2 = exec_units[i].result;
							reservation_stations.entries[k].tag2 = UNDEFINED;
							reservation_stations.entries[k].received_tag_this_cycle = true;
						}
					}
				}
			}
		}

		



		// ----------------------------- ISSUE ----------------------------- 
		if(!branched_this_cycle){


		// issue width handling - loop through issue (width) times
		for(int issue_num = 0; issue_num < issue_width; issue_num++){

		if(pending_instructions.entries[PI_headptr].issue != UNDEFINED || pending_instructions.entries[PI_headptr].released_this_cycle == true) break; // if no room, stop

			//if(pending_instructions.entries[PI_headptr].released_this_cycle == false){

			// get instruction at pc
			IReg = instr_memory[real_pc];

			// eop check
			if(IReg.opcode == EOP) last_instruction_pc = pc - 4;

			// check for structural hazards(free RS/load-store buffer)
			unsigned found_rs = UNDEFINED;
			// search reservation stations by type in ireg - matching type and empty
			if(is_memory(IReg.opcode)){
				bool tag1 = false;
				bool tag2 = false;
				for(unsigned i = 0; i < reservation_stations.num_entries; i++){
					//if(){
					if(reservation_stations.entries[i].instr_exed_this_cycle == false && reservation_stations.entries[i].type == LOAD_B && reservation_stations.entries[i].pc == UNDEFINED){
						found_rs = i;
						reservation_stations.entries[i].address = IReg.immediate;
						reservation_stations.entries[found_rs].destination = ROB_nextindex;//IReg.dest; // entry in rob
						reservation_stations.entries[found_rs].pc = pc;
						//reservation_stations.entries[found_rs].value1 = IReg.immediate;
						//reservation_stations.entries[found_rs].value2 = IReg.src2;
						break;
					}
				}

				for(int tag_id = 0; tag_id < num_units; tag_id++){
					if(instr_memory[(exec_units[tag_id].pc - instr_base_address) / 4].dest == IReg.src1){
						reservation_stations.entries[found_rs].tag1 = instr_memory[(exec_units[tag_id].pc - instr_base_address) / 4].rob_index;
						tag1 = true;
					}
					if(instr_memory[(exec_units[tag_id].pc - instr_base_address) / 4].dest == IReg.src2){
						reservation_stations.entries[found_rs].tag2 = instr_memory[(exec_units[tag_id].pc - instr_base_address) / 4].rob_index;
						tag2 = true;
					}
				}

				if(!tag1) reservation_stations.entries[found_rs].value1 = IReg.immediate;
				if(!tag2) reservation_stations.entries[found_rs].value2 = IReg.src2;
			}



			if(is_int(IReg.opcode)){
				for(unsigned i = 0; i < reservation_stations.num_entries; i++){
					if(reservation_stations.entries[i].instr_exed_this_cycle == false && reservation_stations.entries[i].type == INTEGER_RS && reservation_stations.entries[i].pc == UNDEFINED){
						found_rs = i;
						reservation_stations.entries[found_rs].destination = ROB_nextindex;//IReg.dest; // entry in rob
						reservation_stations.entries[found_rs].pc = pc;
						reservation_stations.entries[found_rs].value1 = IReg.src1;
						reservation_stations.entries[found_rs].value2 = IReg.src2;
						break;
					}
				}
			}
			if(IReg.opcode == ADDS || IReg.opcode == SUBS){
				bool tag1 = false;
				bool tag2 = false;
				for(unsigned i = 0; i < reservation_stations.num_entries; i++){
					if(reservation_stations.entries[i].instr_exed_this_cycle == false && reservation_stations.entries[i].type == ADD_RS && reservation_stations.entries[i].pc == UNDEFINED){
						found_rs = i;
						reservation_stations.entries[found_rs].destination = ROB_nextindex;//IReg.dest; // entry in rob
						reservation_stations.entries[found_rs].pc = pc;
						//reservation_stations.entries[found_rs].value1 = IReg.src1;
						//reservation_stations.entries[found_rs].value2 = IReg.src2;
						break;
					}
				}

				if(get_fp_register_tag(IReg.src1) != UNDEFINED) tag1 = true;
				if(!tag1) {
					reservation_stations.entries[found_rs].value1 = float2unsigned(get_fp_register(IReg.src1));
				} else{
					reservation_stations.entries[found_rs].tag1 = get_fp_register_tag(IReg.src1);
				}

				for(int rob_index = 0; rob_index < rob.num_entries; rob_index++){
						if(rob.entries[rob_index].destination - 32 == IReg.src1 && rob.entries[rob_index].ready == true) {
							reservation_stations.entries[found_rs].value1 = rob.entries[rob_index].value;
							reservation_stations.entries[found_rs].tag1 = UNDEFINED; 	// erase tag
						}
				}


				if(get_fp_register_tag(IReg.src2) != UNDEFINED) tag2 = true;
				if(!tag2) {
					reservation_stations.entries[found_rs].value2 = float2unsigned(get_fp_register(IReg.src2));
				} else{
					reservation_stations.entries[found_rs].tag2 = get_fp_register_tag(IReg.src2);
				}

				for(int rob_index = 0; rob_index < rob.num_entries; rob_index++){
						if(rob.entries[rob_index].destination - 32 == IReg.src2 && rob.entries[rob_index].ready == true) {
							reservation_stations.entries[found_rs].value2 = rob.entries[rob_index].value;
							reservation_stations.entries[found_rs].tag2 = UNDEFINED; 	// erase tag
						}
				}

				
			}

			if(IReg.opcode == MULTS || IReg.opcode == DIVS){
				bool tag1 = false;
				bool tag2 = false;
				for(unsigned i = 0; i < reservation_stations.num_entries; i++){
					if(reservation_stations.entries[i].instr_exed_this_cycle == false && reservation_stations.entries[i].type == MULT_RS && reservation_stations.entries[i].pc == UNDEFINED){
						found_rs = i;
						reservation_stations.entries[found_rs].destination = ROB_nextindex;//IReg.dest; // entry in rob
						reservation_stations.entries[found_rs].pc = pc;
						break;
					}
				}
					// push to RS

				if(get_fp_register_tag(IReg.src1) != UNDEFINED) tag1 = true;
				if(!tag1) {
					reservation_stations.entries[found_rs].value1 = float2unsigned(get_fp_register(IReg.src1));
				} else{
					reservation_stations.entries[found_rs].tag1 = get_fp_register_tag(IReg.src1);
				}

				for(int rob_index = 0; rob_index < rob.num_entries; rob_index++){
						if(rob.entries[rob_index].destination - 32 == IReg.src1 && rob.entries[rob_index].ready == true) {
							reservation_stations.entries[found_rs].value1 = rob.entries[rob_index].value;
							reservation_stations.entries[found_rs].tag1 = UNDEFINED; 	// erase tag
						}
				}


				if(get_fp_register_tag(IReg.src2) != UNDEFINED) tag2 = true;
				if(!tag2) {
					reservation_stations.entries[found_rs].value2 = float2unsigned(get_fp_register(IReg.src2));
				} else{
					reservation_stations.entries[found_rs].tag2 = get_fp_register_tag(IReg.src2);
				}

				for(int rob_index = 0; rob_index < rob.num_entries; rob_index++){
						if(rob.entries[rob_index].destination - 32 == IReg.src2 && rob.entries[rob_index].ready == true) {
							reservation_stations.entries[found_rs].value2 = rob.entries[rob_index].value;
							reservation_stations.entries[found_rs].tag2 = UNDEFINED; 	// erase tag
						}
				}
			}


			if(is_branch(IReg.opcode)){
				bool tag = false;
				for(unsigned i = 0; i < reservation_stations.num_entries; i++){
					if(reservation_stations.entries[i].instr_exed_this_cycle == false && reservation_stations.entries[i].type == INTEGER_RS && reservation_stations.entries[i].pc == UNDEFINED){
						found_rs = i;
						reservation_stations.entries[found_rs].destination = ROB_nextindex;//IReg.dest; // entry in rob
						reservation_stations.entries[found_rs].pc = pc;
						break;
					}
				}

				if(get_int_register_tag(IReg.src1) != UNDEFINED) tag = true;
				if(!tag) {
					reservation_stations.entries[found_rs].value1 = (get_int_register(IReg.src1));
				} else{ 
					reservation_stations.entries[found_rs].tag1 = get_int_register_tag(IReg.src1);
				}

				for(int rob_index = 0; rob_index < rob.num_entries; rob_index++){
						if(rob.entries[rob_index].destination == IReg.src1 && rob.entries[rob_index].ready == true) {
							reservation_stations.entries[found_rs].value1 = rob.entries[rob_index].value;
							reservation_stations.entries[found_rs].tag1 = UNDEFINED; 	// erase tag
						}
				}
			}


			if(found_rs != UNDEFINED){			
			// if found, push to reservation station (how do i set tags)
				/*reservation_stations.entries[found_rs].destination = ROB_nextindex;//IReg.dest; // entry in rob
				reservation_stations.entries[found_rs].pc = pc;
				reservation_stations.entries[found_rs].value1 = IReg.src1;
				if(is_memory(IReg.opcode)) reservation_stations.entries[found_rs].value1 = IReg.immediate;
				reservation_stations.entries[found_rs].value2 = IReg.src2;*/
				// tags
				// set tag of instruction destination equal to rob entry index
				// send values to rob if possible from registers or rob, tags otherwise
				// if register is destination of an instruction currently in an ex unit
				// set that register's matching dest entry on rob as tag


				/*for(int tag_id = 0; tag_id < num_units; tag_id++){
					if(instr_memory[(exec_units[tag_id].pc - instr_base_address) / 4].dest == IReg.src1){
						reservation_stations.entries[found_rs].tag1 = instr_memory[(exec_units[tag_id].pc - instr_base_address) / 4].rob_index;
					}
					if(instr_memory[(exec_units[tag_id].pc - instr_base_address) / 4].dest == IReg.src2){
						reservation_stations.entries[found_rs].tag2 = instr_memory[(exec_units[tag_id].pc - instr_base_address) / 4].rob_index;
					}
				}*/


				/*	if() reservation_stations.entries[found_rs].tag1 = ROB_nextindex;
				if() reservation_stations.entries[found_rs].tag2 = ROB_nextindex;*/
			
			// If we found a reservation station & pending instruction:
				// Push to ROB 
				if(!is_branch(IReg.opcode))rob.entries[ROB_nextindex].destination = IReg.dest + NUM_GP_REGISTERS;
				if(is_int(IReg.opcode)) rob.entries[ROB_nextindex].destination = IReg.dest; //fp

				//reservation_stations.entries[found_rs].instr_exed_this_cycle = 

				rob.entries[ROB_nextindex].pc = pc;
				rob.entries[ROB_nextindex].ready = false;
				rob.entries[ROB_nextindex].state = ISSUE;
				rob.entries[ROB_nextindex].value = UNDEFINED;
			
				// Push to pending instruction list
				pending_instructions.entries[PI_headptr].pc = pc;
				pending_instructions.entries[PI_headptr].issue = clock_cycles;
				instr_memory[real_pc].pending_index = PI_headptr; 
				instr_memory[real_pc].rob_index = ROB_nextindex;

				ROB_nextindex++;
				if(ROB_nextindex == rob.num_entries) ROB_nextindex = 0;
				PI_headptr++;
				if(PI_headptr == pending_instructions.num_entries) PI_headptr = 0;
				real_pc++;
				pc = real_pc * 4 + instr_base_address;
			}

			// if no free reservation station (structural hazard), 
			// can't do anything - don't advance pc
		// --------------------------- END ISSUE --------------------------- 
		//}
		}
	}







	// bandaids (squashed memory problems)
	for(unsigned i = 0; i < pending_instructions.num_entries; i++){
		if(pending_instructions.entries[i].pc == UNDEFINED) reset_pending_instruction(i);
		if(pending_instructions.entries[i].wr == UNDEFINED) pending_instructions.entries[i].commit = UNDEFINED;
		if(pending_instructions.entries[i].commit >= 1000000) pending_instructions.entries[i].commit = UNDEFINED;
	}
	if(int_fp_registers[31] < -2000000000) int_fp_registers[31] = UNDEFINED;










	}
}

//reset the state of the simulator - please complete
void sim_ooo::reset(){

	//init instruction log
	init_log();	

	// data memory
	for (unsigned i=0; i<data_memory_size; i++) data_memory[i]=0xFF;
	
	//instr memory
	for (int i=0; i<PROGRAM_SIZE;i++){
		instr_memory[i].opcode=(opcode_t)EOP;
		instr_memory[i].src1=UNDEFINED;
		instr_memory[i].src2=UNDEFINED;
		instr_memory[i].dest=UNDEFINED;
		instr_memory[i].immediate=UNDEFINED;
		instr_memory[i].pending_index = UNDEFINED; // new
		instr_memory[i].rob_index = UNDEFINED; // new
	}

	//general purpose registers
	for(int i = 0; i < NUM_GP_REGISTERS; i++){
		//fp_registers[i] = UNDEFINED;
		int_fp_registers[i] = (int)UNDEFINED;
	}
	for(int i = 0; i < NUM_GP_REGISTERS; i++){
		//fp_registers[i] = UNDEFINED;
		set_fp_register(i,UNDEFINED);
	}
	//pending_instructions
	for(unsigned i = 0; i < pending_instructions.num_entries;i++){
		reset_pending_instruction(i);
	}

	//rob
	for(unsigned i=0; i< rob.num_entries;i++){
		//rob_entry_t * current = rob.entries[i];
		clean_rob(&rob.entries[i]);
	}
	//reservation_stations
	for(unsigned i=0; i< reservation_stations.num_entries;i++){
		reset_reservation_station(i);
	}


	//execution statistics
	clock_cycles = -1;
	instructions_executed = 0;
	

	//other required initializations
	null_inst.dest = UNDEFINED;
	null_inst.immediate = UNDEFINED;
	//null_inst.label = "";
	null_inst.opcode = NOP;
	null_inst.src1 = UNDEFINED;
	null_inst.src2 = UNDEFINED;
	null_inst.pending_index = UNDEFINED;
	null_inst.rob_index = UNDEFINED;

	IReg.dest = UNDEFINED;
	IReg.immediate = UNDEFINED;
	//null_inst.label = "";
	IReg.opcode = NOP;
	IReg.src1 = UNDEFINED;
	IReg.src2 = UNDEFINED;
	IReg.pending_index = UNDEFINED;
	IReg.rob_index = UNDEFINED;
	
	real_pc = 0;
	ROB_headptr = 0;
	PI_headptr = 0;
	ROB_nextindex = 0;

	last_instruction_pc = UNDEFINED;
}

/* registers related */

int sim_ooo::get_int_register(unsigned reg){
	return int_fp_registers[reg]; 
}

void sim_ooo::set_int_register(unsigned reg, int value){
	int_fp_registers[reg] = (float)value;
}

float sim_ooo::get_fp_register(unsigned reg){
	return int_fp_registers[reg + NUM_GP_REGISTERS];
}

void sim_ooo::set_fp_register(unsigned reg, float value){
	int_fp_registers[reg + NUM_GP_REGISTERS] = value;
}

unsigned sim_ooo::get_int_register_tag(unsigned reg){ // for now, get_*_register_tag functions return value of register
	return UNDEFINED; //please modify
}

unsigned sim_ooo::get_fp_register_tag(unsigned reg){

	// find latest issued instruction on pending
	int current_latest_cycle = UNDEFINED;
	unsigned pindex = UNDEFINED;
	unsigned rval = UNDEFINED;
	for(int i = 0; i < pending_instructions.num_entries; i++){
		if(instr_memory[(pending_instructions.entries[i].pc - instr_base_address) / 4].dest == reg && ((int)pending_instructions.entries[i].issue > (int)current_latest_cycle)){
			current_latest_cycle = pending_instructions.entries[i].issue;
			pindex = i;
		}
	}
	//if(pindex == UNDEFINED) return UNDEFINED;
	
	if(pindex != UNDEFINED) rval = (instr_memory[(pending_instructions.entries[pindex].pc - instr_base_address)/4].rob_index);
	return rval;
	// return (instr_memory[pending index . pc]).rob_index



	/*
	// set tag of instruction destination equal to rob entry index :)
	//return UNDEFINED; //please modify
	unsigned rval = UNDEFINED;
	unsigned target = reg + NUM_GP_REGISTERS;
	//unsigned 
	unsigned issue_cycle = 0;
	stage_t rloc;
	for(int j = 0; j < rob.num_entries; j++){
		unsigned compare = 0;
		if(rob.entries[j].pc != UNDEFINED) compare = pending_instructions.entries[instr_memory[(rob.entries[j].pc - instr_base_address)/4].pending_index].issue;
		if(rob.entries[j].destination == target && compare > issue_cycle) {
			rval = j;
			//rloc = rob.entries[j].state;
			//issue_cycle = 
			
		}
	}
	return rval;*/
}

void sim_ooo::reset_pending_instruction(unsigned i){
		pending_instructions.entries[i].commit = UNDEFINED;
		pending_instructions.entries[i].exe = UNDEFINED;
		pending_instructions.entries[i].issue = UNDEFINED;
		pending_instructions.entries[i].pc = UNDEFINED;
		pending_instructions.entries[i].wr = UNDEFINED;
		pending_instructions.entries[i].released_this_cycle = false;
}

void sim_ooo::reset_reservation_station(unsigned i){
	reservation_stations.entries[i].address = UNDEFINED;
	reservation_stations.entries[i].destination = UNDEFINED;
	reservation_stations.entries[i].pc = UNDEFINED;
	reservation_stations.entries[i].tag1 = UNDEFINED;
	reservation_stations.entries[i].tag2 = UNDEFINED;
	reservation_stations.entries[i].value1 = UNDEFINED;
	reservation_stations.entries[i].value2 = UNDEFINED;
	reservation_stations.entries[i].received_tag_this_cycle = false;
	reservation_stations.entries[i].instr_exed_this_cycle = false;
}



bool sim_ooo::isALUorSTORE(instruction_t i){
	return (
		i.opcode == SWS ||
		i.opcode == SW ||
		i.opcode == ADD ||
		i.opcode == ADDI ||
		i.opcode == SUB ||
		i.opcode == SUBI ||
		i.opcode == XOR ||
		i.opcode == AND ||
		i.opcode == MULT ||
		i.opcode == DIV ||
		i.opcode == SWS ||
		i.opcode == ADDS ||
		i.opcode == SUBS ||
		i.opcode == MULTS ||
		i.opcode == DIVS
	);
}


bool sim_ooo::isBRANCH(instruction_t i){
	return(
		i.opcode == BEQZ ||
		i.opcode == BGEZ ||
		i.opcode == BGTZ ||
		i.opcode == BLEZ || 
		i.opcode == BLTZ ||
		i.opcode == BNEZ
	);
}