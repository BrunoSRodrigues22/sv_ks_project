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
    
    typedef enum logic [3:0]{
        fetch,
        decode,
        prox,
        prox1,
        load1,
        end_program
    } state_t;
    
    state_t state;
    
    state_t next_state;
    
    always @(posedge clk) begin : clk_ctrl 
        if(rst_n == 1'b0)
            state <= fetch;
         else 
            state <= next_state;
    end : clk_ctrl

    always @(posedge clk) begin : state_ctrl
        next_state <= state;
        
        unique case(state)
           fetch : begin
                addr_sel         <= 1'b1;
                c_sel            <= 1'b0;
                ir_enable        <= 1'b1;
                flags_reg_enable <= 1'b0;
                pc_enable        <= 1'b0;
                write_reg_enable <= 1'b0;
                halt             <= 1'b0;
                next_state       <= decode;
           end
           
           decode : begin
                ir_enable <= 1'b0;
                
                unique case(decoded_instruction)
                    I_NOP : begin
                        ir_enable        <= 1'b0;
                        flags_reg_enable <= 1'b0;
                        branch           <= 1'b0;
                        pc_enable        <= 1'b0;
                        halt             <= 1'b0;
                        write_reg_enable <= 1'b0;
                        next_state       <= prox;
                        //state <= prox;
                    end
                    
                    I_LOAD : begin
                        ir_enable        <= 1'b0;
                        flags_reg_enable <= 1'b0;
                        addr_sel         <= 1'b0;
                        branch           <= 1'b0;
                        halt             <= 1'b0;
                        //write_reg_enable <= 1'b1;
                        write_reg_enable <= 1'b0;
                        ///c_sel            <= 1'b1;
                        
                        next_state       <= load1;
                        //state <=load1;
                    end
                    
                    I_STORE : begin
                        addr_sel         <= 1'b0;
                        ram_write_enable <= 1'b1;
                        next_state       <= prox;
                        //state <=prox;
                    end
                    
                    I_MOVE : begin
                        ir_enable        <= 1'b0;
                        flags_reg_enable <= 1'b0;
                        operation        <= 2'b00;
                        c_sel            <= 1'b0;
                        halt             <= 1'b0;
                        write_reg_enable <= 1'b1;
                        next_state       <= prox;
                        //state <=prox;
                    end
                    
                    I_ADD : begin
                        operation        <= 2'b01;
                        c_sel            <= 1'b0;
                        write_reg_enable <= 1'b1;
                        ir_enable        <= 1'b0;
                        flags_reg_enable <= 1'b1;
                        next_state       <= prox;
                        //state <=prox;
                    end
                    
                    I_SUB : begin
                        operation        <= 2'b10;
                        c_sel            <= 1'b0;
                        write_reg_enable <= 1'b1;
                        ir_enable        <= 1'b0;
                        flags_reg_enable <= 1'b1;
                        next_state       <= prox;
                        //state <=prox;
                    end
                    
                    I_AND : begin
                        operation        <= 2'b11;
                        c_sel            <= 1'b0;
                        write_reg_enable <= 1'b1;
                        ir_enable        <= 1'b0;
                        flags_reg_enable <= 1'b1;
                        next_state       <= prox;
                        //state <=prox;
                    end
                    
                    I_OR : begin
                        operation        <= 2'b00;
                        c_sel            <= 1'b0;
                        write_reg_enable <= 1'b1;
                        ir_enable        <= 1'b0;
                        flags_reg_enable <= 1'b1;
                        next_state       <= prox;
                        //state <=prox;
                    end
                    
                    I_BRANCH : begin
                        branch           <= 1'b1;
                        ir_enable        <= 1'b0;
                        flags_reg_enable <= 1'b0;
                        addr_sel         <= 1'b0;
                        next_state       <= prox;
                        //state <=prox;
                    end
                    
                    I_BZERO : begin
                        if(zero_op == 1'b1) begin
                            branch           <= 1'b1;
                            ir_enable        <= 1'b0;
                            flags_reg_enable <= 1'b0;
                            addr_sel         <= 1'b0;
                            next_state       <= prox;
                            //state <=prox;
                        end else
                            next_state       <= prox;
                            //state <=prox;
                    end
                    
                    I_BNZERO : begin
                        if(zero_op == 1'b0) begin
                            branch           <= 1'b1;
                            ir_enable        <= 1'b0;
                            flags_reg_enable <= 1'b0;
                            addr_sel         <= 1'b0;
                            next_state       <= prox;
                            //state <=prox;
                        end else
                            next_state       <= prox;
                            //state <=prox;
                    end
                    
                    I_BNEG : begin
                        if(neg_op == 1'b1) begin
                            branch           <= 1'b1;
                            ir_enable        <= 1'b0;
                            flags_reg_enable <= 1'b0;
                            addr_sel         <= 1'b0;
                            next_state       <= prox;
                            //state <=prox;
                        end else
                            next_state       <= prox;
                            //state <=prox;
                    end
                    
                    I_BNNEG : begin
                        if(neg_op == 1'b0) begin
                            branch           <= 1'b1;
                            ir_enable        <= 1'b0;
                            flags_reg_enable <= 1'b0;
                            addr_sel         <= 1'b0;
                            next_state       <= prox;
                            //state <=prox;
                        end else
                            next_state       <= prox;
                            //state <=prox;
                    end
                    
                    I_HALT : begin
                        ir_enable        <= 1'b0;
                        flags_reg_enable <= 1'b0;
                        branch           <= 1'b0;
                        pc_enable        <= 1'b0;
                        write_reg_enable <= 1'b0;
                        halt             <= 1'b1;
                        next_state       <= end_program;
                        //state <=end_program;
                    end
                endcase
           end
           
           prox : begin
                ir_enable        <= 1'b0;
                flags_reg_enable <= 1'b0;
                pc_enable        <= 1'b1;
                addr_sel         <= 1'b1;
                halt             <= 1'b0;
                write_reg_enable <= 1'b0;
                ram_write_enable <= 1'b0;
                next_state       <= prox1;
                //state <= prox1;
           end
           
           prox1 : begin
                branch           <= 1'b0;
                ir_enable        <= 1'b0;
                flags_reg_enable <= 1'b0;
                pc_enable        <= 1'b0;
                halt             <= 1'b0;
                write_reg_enable <= 1'b0;
                next_state       <= fetch;
                //state <=fetch;
           end
           
           load1: begin
                c_sel <= 1'b1;
                write_reg_enable <= 1'b1;
                next_state <= prox;
                //state <=prox;
           end
           
           end_program : begin         
                next_state <= end_program;
                //state <= end_program;
           end
        endcase
        
    end : state_ctrl
    

endmodule : control_unit
