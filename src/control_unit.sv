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
    
    typedef enum logic [2:0]{
            FETCH
        ,   DECODE
        ,   NEXT_INST
        ,   RESET
        ,   ALU_CTRL
        ,   BRANCH_CTRL
        ,   END_PROGRAM
    } state_t;
    
    state_t state, next_state;
    
    always @(posedge clk) begin : clk_ctrl 
        if(rst_n == 1'b0)
            state <= FETCH;
         else 
            state <= next_state;
    end : clk_ctrl

    always @(posedge clk) begin : state_ctrl
        next_state <= state;
        
        unique case(state)
           FETCH : begin
                addr_sel         <= 1'b1;
                c_sel            <= 1'b0;
                ir_enable        <= 1'b1;
                flags_reg_enable <= 1'b0;
                pc_enable        <= 1'b0;
                write_reg_enable <= 1'b0;
                halt             <= 1'b0;
                next_state       <= DECODE;
           end
           
           DECODE : begin
                ir_enable <= 1'b0;
                
                unique case(decoded_instruction)
                    I_NOP : next_state <= NEXT_INST;
                        
                    I_LOAD : begin
                        ir_enable        <= 1'b0;
                        flags_reg_enable <= 1'b0;
                        addr_sel         <= 1'b0;
                        branch           <= 1'b0;
                        halt             <= 1'b0;
                        write_reg_enable <= 1'b1;
                        c_sel            <= 1'b1;
                        next_state       <= NEXT_INST;
                    end
                    
                    I_STORE : begin
                        addr_sel         <= 1'b0;
                        ram_write_enable <= 1'b1;
                        next_state       <= NEXT_INST;
                    end
                    
                    I_MOVE : begin
                        ir_enable        <= 1'b0;
                        flags_reg_enable <= 1'b0;
                        operation        <= 2'b00;
                        c_sel            <= 1'b0;
                        halt             <= 1'b0;
                        write_reg_enable <= 1'b1;
                        next_state       <= NEXT_INST;
                    end
                    
                    I_ADD : begin
                        operation  <= 2'b01;
                        next_state <= ALU_CTRL;
                    end
                    
                    I_SUB : begin
                        operation  <= 2'b10;
                        next_state <= ALU_CTRL;
                    end
                    
                    I_AND : begin
                        operation  <= 2'b11;
                        next_state <= ALU_CTRL;
                    end
                    
                    I_OR : begin
                        operation  <= 2'b00;
                        next_state <= ALU_CTRL;
                    end
                    
                    I_BRANCH : begin
                        next_state <= BRANCH_CTRL;
                    end
                    
                    I_BZERO : begin
                        if(zero_op == 1'b1)
                            next_state <= BRANCH_CTRL;
                        else
                            next_state <= NEXT_INST;
                    end
                    
                    I_BNZERO : begin
                        if(zero_op == 1'b0)
                            next_state <= BRANCH_CTRL;
                        else
                            next_state <= NEXT_INST;
                    end
                    
                    I_BNEG : begin
                        if(neg_op == 1'b1)
                            next_state <= BRANCH_CTRL;
                        else
                            next_state <= NEXT_INST;
                    end
                    
                    I_BNNEG : begin
                        if(neg_op == 1'b0)
                            next_state <= BRANCH_CTRL;
                        else
                            next_state <= NEXT_INST;
                    end
                    
                    I_HALT : begin
                        ir_enable        <= 1'b0;
                        flags_reg_enable <= 1'b0;
                        branch           <= 1'b0;
                        pc_enable        <= 1'b0;
                        write_reg_enable <= 1'b0;
                        halt             <= 1'b1;
                        next_state       <= END_PROGRAM;
                    end
                endcase
           end
           
           ALU_CTRL : begin
                c_sel            <= 1'b0;
                write_reg_enable <= 1'b1;
                ir_enable        <= 1'b0;
                flags_reg_enable <= 1'b1;
                next_state       <= NEXT_INST;
           end
           
           BRANCH_CTRL : begin
                branch           <= 1'b1;
                ir_enable        <= 1'b0;
                flags_reg_enable <= 1'b0;
                addr_sel         <= 1'b0;
                next_state       <= NEXT_INST;
           end
           
           NEXT_INST : begin
                ir_enable        <= 1'b0;
                flags_reg_enable <= 1'b0;
                pc_enable        <= 1'b1;
                addr_sel         <= 1'b1;
                halt             <= 1'b0;
                write_reg_enable <= 1'b0;
                ram_write_enable <= 1'b0;
                next_state       <= RESET;
           end
           
           RESET : begin
                branch           <= 1'b0;
                ir_enable        <= 1'b0;
                flags_reg_enable <= 1'b0;
                pc_enable        <= 1'b0;
                halt             <= 1'b0;
                write_reg_enable <= 1'b0;
                next_state       <= FETCH;
           end
           
           END_PROGRAM : begin         
                next_state <= END_PROGRAM;
           end
        endcase
        
    end : state_ctrl
    

endmodule : control_unit
