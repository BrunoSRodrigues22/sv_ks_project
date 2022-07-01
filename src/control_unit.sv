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
    
    logic [1:0] state;
    logic fetch = 2'b00, decode = 2'b01, exec = 2'b10, wr_back = 2'b11; 

    always @(posedge clk) begin : state_ctrl
        unique case(state)
           fetch : begin
                pc_enable        <= 1'b1;
                addr_sel         <= 1'b1;
                ir_enable        <= 1'b1;
                c_sel            <= 1'b0;
                branch           <= 1'b0;
                ram_write_enable <= 1'b0;
                flags_reg_enable <= 1'b0;
                write_reg_enable <= 1'b0;
                halt             <= 1'b0;
                state            <= decode;
           end
           
           decode : begin
                ir_enable <= 1'b0;
                addr_sel  <= 1'b0;
                c_sel     <= 1'b0;
                pc_enable <= 1'b0;
                branch    <= 1'b0;
                
                unique case(decoded_instruction)
                    I_NOP : begin
                        state <= fetch;
                    end
                    
                    I_LOAD : begin
                        
                    end
                    
                    I_STORE : begin
                    
                    end
                    
                    I_MOVE : begin
                    
                    end
                    
                    I_ADD : begin
                        operation <= 2'b00;
                    end
                    
                    I_SUB : begin
                        operation <= 2'b01;
                    end
                    
                    I_AND : begin
                        operation <= 2'b10;
                    end
                    
                    I_OR : begin
                        operation <= 2'b11;
                    end
                    
                    I_BRANCH : begin
                    
                    end
                    
                    I_BZERO : begin
                    
                    end
                    
                    I_BNZERO : begin
                    
                    end
                    
                    I_BNZERO : begin
                    
                    end
                    
                    I_BNEG : begin
                    
                    end
                    
                    I_BNNEG : begin
                    
                    end
                    
                    I_HALT : begin
                        halt <= 1'b1;
                        //insert end
                    end
                endcase
                
                state     <= exec;
           end
           
           exec : begin
                
           end
        endcase
    end : state_ctrl
 
    
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
