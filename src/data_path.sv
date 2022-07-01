module data_path
import k_and_s_pkg::*;
(
    input  logic                    rst_n,
    input  logic                    clk,
    input  logic                    branch,
    input  logic                    pc_enable,
    input  logic                    ir_enable,
    input  logic                    addr_sel,
    input  logic                    c_sel,
    input  logic              [1:0] operation,
    input  logic                    write_reg_enable,
    input  logic                    flags_reg_enable,
    output decoded_instruction_type decoded_instruction,
    output logic                    zero_op,
    output logic                    neg_op,
    output logic                    unsigned_overflow,
    output logic                    signed_overflow,
    output logic              [4:0] ram_addr,
    output logic             [15:0] data_out,
    input  logic             [15:0] data_in

);
    
    logic  [1:0] a_addr, b_addr, c_addr;//Address_registers
    logic [15:0] bus_a, bus_b, bus_c, alu_out;//Bus address
    logic [15:0] reg_bank[3];
    logic [15:0] instruction;
    logic  [4:0] program_counter;
    logic  [4:0] mem_addr;
    logic        zero, neg, unsign, sign;//Branch_Detect
    
    always_ff @(posedge clk) begin : ir_ctrl
        if(ir_enable)
            instruction[15:0] <= data_in[15:0];
    end : ir_ctrl
    
    always_comb begin : decode_comand
        unique case(instruction[8:0])
            'b000000000 : decoded_instruction <= I_NOP;
            'b100000010 : decoded_instruction <= I_LOAD;
            'b100000100 : decoded_instruction <= I_STORE;
            'b100100010 : decoded_instruction <= I_MOVE;
            'b101000010 : decoded_instruction <= I_ADD;
            'b101000100 : decoded_instruction <= I_SUB;
            'b101000110 : decoded_instruction <= I_AND;
            'b101001000 : decoded_instruction <= I_OR;
            'b000000010 : decoded_instruction <= I_BRANCH;
            'b000000100 : decoded_instruction <= I_BZERO;
            'b000010110 : decoded_instruction <= I_BNZERO;
            'b000000110 : decoded_instruction <= I_BNEG;
            'b000010100 : decoded_instruction <= I_BNNEG;
            'b111111111 : decoded_instruction <= I_HALT;
        endcase
    end : decode_comand
  
    always_ff @(posedge clk) begin : write_reg_enable_ctrl
        if(write_reg_enable)
            reg_bank[c_addr] <= bus_c;
            bus_a            <= reg_bank[a_addr]; 
            bus_b            <= reg_bank[b_addr];
            data_out         <= bus_a; 
    end : write_reg_enable_ctrl
    
    always_comb begin: ula_control
        unique case(operation)
            2'b00 : alu_out = bus_a + bus_b;
            2'b01 : alu_out = bus_a - bus_b;
            2'b10 : alu_out = bus_a & bus_b;
            2'b11 : alu_out = bus_a | bus_b;
        endcase
    end : ula_control
    
    
    
    assign    neg = (alu_out[15]==1'b1)?'b1:'b0;
    //assign zero = ~|(alu_out);
    assign   zero = (alu_out[15:0]=='b0)?'b1:'b0;
    assign unsign = (neg=='b0)?'b1:'b0;
    assign   sign = (neg=='b1)?'b1:'b0;
    
    //assign zero_op = ~|(alu_out);
    //assign neg_op = alu_out[15];
    
    always_ff @(posedge clk) begin : flag_reg
        if(flags_reg_enable)
            zero_op           <= zero;
            neg_op            <= neg;
            unsigned_overflow <= unsign;
            signed_overflow   <= sign;
    end : flag_reg
    
    assign bus_c = (c_sel==1'b1)?data_in:alu_out;
    
    assign program_counter = (branch==1'b1)?mem_addr:program_counter;
    
    always_ff @(posedge clk) begin : reg_pc
        if(pc_enable)
            if(~rst_n)
                program_counter <= 'b0;
            else
                program_counter = program_counter + 1;
    end : reg_pc
    
    
    assign ram_addr = (addr_sel==1'b1)?mem_addr:program_counter; 

endmodule : data_path
