# RISC CPU

Description: In this project I created a CPU. The top level design for this CPU consists of 5 registers. Information gets stored to the registers from the ALU. Source A and B go into the ALU where the appropriate operation is completed then they are stored at the specified destination. The ROM is 256 potential entires that are 16 bits long so the address is an 8 bit address. The RAM is also 256 16 bit slots and this is the stack and additional memory. The MAR holds the address and the MBR holds the data that is going in. The input port takes information in and the output port takes information out. The IR holds the instructions that come from the ROM. the PC returns an address that goes into the ROM. The stack pointer keeps track of the length of the stack. The CR reflects the state. There are also holders for the RAM output and the operations that indicate the ALU operation.
The contents of the RAM are 256 potential entries that are 16 bits in length. There is a one bit write in, a 16 bit data in, an eight bit address, and a 16 bit data out. This is the stack and additional memory for the CPU. The ROM also has 256 potential entries that are 16 bits in length a 16 bit data out and an eight bit address.
