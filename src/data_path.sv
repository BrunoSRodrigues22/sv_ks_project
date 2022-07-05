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
    logic [15:0] reg_bank[3:0];
    logic [15:0] instruction;
    logic  [4:0] program_counter;
    logic  [4:0] mem_addr;
    
    assign ram_addr = (addr_sel==1'b0)?mem_addr:program_counter;
    
    always_ff @(posedge clk) begin : ir_ctrl
        if(ir_enable)
            instruction[15:0] <= data_in[15:0];
    end : ir_ctrl
    
    always_comb begin : decode_comand
        unique case(instruction[15:7])
            9'b000000000 : begin
                decoded_instruction <= I_NOP;
                mem_addr            <= instruction[4:0];
            end
            
            9'b100000010 : begin
                decoded_instruction <= I_LOAD;
                c_addr              <= instruction[6:5];
                b_addr              <= instruction[1:0];
                a_addr              <= instruction[3:2];
            end
            
            9'b100000100 : begin
                decoded_instruction <= I_STORE;
                a_addr              <= instruction[6:5];
            end
            
            9'b100100010 : begin
                decoded_instruction <= I_MOVE;
                c_addr              <= instruction[3:2];
                b_addr              <= instruction[1:0];
                a_addr              <= instruction[1:0];
            end
              
            9'b101000010 : begin 
                decoded_instruction <= I_ADD;
                c_addr              <= instruction[5:4];
                b_addr              <= instruction[1:0];
                a_addr              <= instruction[3:2];
            end
            
            9'b101000100 : begin
                decoded_instruction <= I_SUB;
                c_addr              <= instruction[5:4];
                b_addr              <= instruction[1:0];
                a_addr              <= instruction[3:2];
            end
            
            9'b101000110 : begin
                decoded_instruction <= I_AND;
                c_addr              <= instruction[5:4];
                b_addr              <= instruction[1:0];
                a_addr              <= instruction[3:2];
            end
            
            9'b101001000 : begin
                decoded_instruction <= I_OR;
                c_addr              <= instruction[5:4];
                b_addr              <= instruction[1:0];
                a_addr              <= instruction[3:2];
            end
            
            9'b000000010 : begin
                decoded_instruction <= I_BRANCH;
                mem_addr            <= instruction[4:0];
            end
            
            9'b000000100 : begin
                decoded_instruction <= I_BZERO;
                mem_addr            <= instruction[4:0];
            end
            
            9'b000010110 : begin
                decoded_instruction <= I_BNZERO;
                mem_addr            <= instruction[4:0];
            end
            
            9'b000000110 : begin
                decoded_instruction <= I_BNEG;
                mem_addr            <= instruction[4:0];
            end
            
            9'b000010100 : begin
                decoded_instruction <= I_BNNEG;
                mem_addr            <= instruction[4:0];
            end
            
            9'b111111111 : begin
                decoded_instruction <= I_HALT;
            end
        endcase
    end : decode_comand
  
    always_ff @(posedge clk) begin : write_reg_enable_ctrl
        if(write_reg_enable) begin
            reg_bank[a_addr] <= bus_a;
            reg_bank[b_addr] <= bus_b;
            reg_bank[c_addr] <= bus_c;
        end            
    end : write_reg_enable_ctrl
    
    always_comb begin: ula_control
            bus_a    <= reg_bank[a_addr];
            bus_b    <= reg_bank[b_addr];
            data_out <= bus_a;
            
        unique case(operation)
            2'b00 : alu_out = bus_a || bus_b;
            2'b01 : alu_out = bus_a + bus_b;
            2'b10 : alu_out = bus_a - bus_b;
            2'b11 : alu_out = bus_a && bus_b;
        endcase
    end : ula_control
    
    assign bus_c = (c_sel==1'b0)?alu_out:data_in;
    
    always_ff @(posedge clk) begin : flag_reg
        if(flags_reg_enable) begin
            neg_op            <= (alu_out[15]==1'b1)?1'b1:1'b0; 
            zero_op           <= (alu_out[15:0]==1'b0)?1'b1:1'b0;
            unsigned_overflow <= (neg_op==1'b0)?1'b1:1'b0;
            signed_overflow   <= (neg_op==1'b1)?1'b1:1'b0;
        end else begin
            neg_op            <= 'b0;
            zero_op           <= 'b0;
            unsigned_overflow <= 'b0;
            signed_overflow   <= 'b0;
        end
    end : flag_reg
    
    always_ff @(posedge clk) begin : reg_pc
            if(~rst_n)
                program_counter <= 'b0;
            else
                if(pc_enable == 1'b1) begin
                    if(branch == 1'b1)
                        program_counter <= mem_addr;
                    else
                        program_counter <= program_counter + 1;
                end
    end : reg_pc
    
endmodule : data_path
