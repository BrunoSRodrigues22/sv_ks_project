//control unit
module control_unit
import k_and_s_pkg::*;
(
    input  logic                    rst_n,
    input  logic                    clk,
    output logic                    branch,
    output logic                    pc_enable,
    output logic                    ir_enable,
    output logic                    write_reg_enable,
    output logic                    addr_sel,
    output logic                    c_sel,
    output logic              [1:0] operation,
    output logic                    flags_reg_enable,
    input  decoded_instruction_type decoded_instruction,
    input  logic                    zero_op,
    input  logic                    neg_op,
    input  logic                    unsigned_overflow,
    input  logic                    signed_overflow,
    output logic                    ram_write_enable,
    output logic                    halt
);

    logic node;
    logic [4:0] program_counter;//Register PC
    logic [15:0] instruction;//Register IR
    
    always_comb begin : Instruction_decoder
        unique case(instruction)
             
        endcase
    end : Instruction_decoder
    
    always @(posedge clk or negedge rst_n) begin 
        if(~rst_n)
            program_counter <= 'd0;
        else
            program_counter <= program_counter + 1;
    end
    
    assign node = instruction[15];
    
    //assign decoded_instruction = (node==1'b1)?'d0:instruction[]
    
    
    logic [4:0] mem_addr;
    
    //assign ram_write_enable = 
    
    assign halt = ( (&(program_counter))?1'b1:1'b0);
    
    //assign addr = (addr_sel==1'b1)?program_counter:mem_addr;//Mux_ctrl

    //assign instruction = 
    
/*
    logic [7:0] counter = 'd0;

    always @(posedge clk or negedge rst_n) begin
        if (~rst_n)
            counter <= 'd0;
        else
            counter <= counter + 1;
    end

    assign halt = ( (&(counter)) ? 1'b1 : 1'b0);
*/

endmodule : control_unit
