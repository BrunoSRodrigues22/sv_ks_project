module data_path
import k_and_s_pkg::*;
(
    input  logic                    rst_n,
    input  logic                    clk,
    input  logic                    branch,
    input  logic                    pc_enable,
    input  logic                    ir_enable,
    input  logic                    addr_sel,//ok
    input  logic                    c_sel,
    input  logic              [1:0] operation,//ok
    input  logic                    write_reg_enable,
    input  logic                    flags_reg_enable,
    output decoded_instruction_type [4:0] decoded_instruction,
    output logic                    zero_op,//ok
    output logic                    neg_op,//ok
    output logic                    unsigned_overflow,
    output logic                    signed_overflow,
    output logic              [4:0] ram_addr,//ok
    output logic             [15:0] data_out,
    input  logic             [15:0] data_in

);
    
    //start fetch  
    logic [15:0] instruction;
    assign instruction = (ir_enable==1'b1)?data_in:rst_n;
    //endfetch
    
    
    //Start_decode
        //assign a_addr = instruction[15:14];//A Addr;
    //assign b_addr = instruction[13:12];//B Addr;
    //assign operation = instruction[11:10];//ALU Op;
        
        //assign decoded_instruction = instruction[9:7];//Switch Pos;
        //assign c_addr = instruction[6:5];//C Addr;
        //assign mem_addr = instruction[4:0];//RW Addr;
    //End_decode
    
    
    //Operation_Config
    logic [1:0] a_addr, b_addr, c_addr;//Address_registers
    logic [15:0] reg_bank[3];
    logic [15:0] bus_a, bus_b, bus_c, alu_out;//Bus address
  
    always_ff @(posedge clk) begin : write_reg_enable_ctrl
        if(write_reg_enable)
            reg_bank[c_addr] <= bus_c;
    end : write_reg_enable_ctrl
    
    assign bus_a = reg_bank[a_addr]; 
    assign bus_b = reg_bank[b_addr];
    
    always_comb begin: ula_control
        unique case(operation)
            2'b00 : alu_out = bus_a + bus_b;
            2'b01 : alu_out = bus_a - bus_b;
            2'b10 : alu_out = bus_a & bus_b;
            2'b11 : alu_out = bus_a | bus_b;
        endcase
    end : ula_control
    
    assign bus_c = (c_sel==1'b1)?alu_out:data_in;//Mux_ctrl
    
    assign data_out = bus_a;
    
    //assign neg = alu_out[15];
    //assign zero = ~|(alu_out);
    
    //assign zero_op = ~|(alu_out);
    //assign neg_op = alu_out[15];
    
    
    //Program_Counter_Config
    logic [4:0] program_counter;
    logic [4:0] mem_addr;
    
    assign program_counter = (branch==1'b1)?mem_addr:program_counter;
    
    always_ff @(posedge clk) begin : reg_pc
        if(pc_enable)
            program_counter = program_counter + 1;
    end : reg_pc
    
    assign ram_addr = (addr_sel==1'b1)?program_counter:mem_addr;
    
    /*always_ff @(posedge clk) begin : flag_reg
        if(flags_reg_enable)
            //if(
            assign neg_op = (neg==1'b1)?neg:1'b0;
            assign zero_op = (zero==1'b1)?zero:1'b0;
    end : flag_reg*/

    //assign ram_addr = 'd0; 

endmodule : data_path
