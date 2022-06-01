/*************************************************************************************************************
**
**filename : qspi_core
**
**Project  : quad spi interface for flash
**
**Purpose  : Quad SPI interface for flash available on the arty a7-35t FPGA board
**
**Creator  : Muhammad Waleed Waseem
**
**Email Address : waleed.waseem99@gmail.com
**
**************************************************************************************************************/


module qspi_core #(
    parameter A_WIDTH = 24,
    parameter D_WIDTH = 32
)(
    input logic clk_i,
    input logic rst_ni,
// Bus interface 
    input  logic               we_i,
    input  logic               re_i,
    input  logic [A_WIDTH-1:0] addr_i,
    input  logic [D_WIDTH-1:0] wdata_i,
    output logic [D_WIDTH-1:0] rdata_o,
// QSPI interface
    output logic       qsclk_o,
    output logic       qcsb_o,
    input  logic [3:0] qspi_i,
    output logic [3:0] qspi_o,
    output logic [3:0] qspi_oeb
);

// qspi states
typedef enum logic [1:0] {IDLE, READ, WRITE, XIP} main_state_t;
typedef enum logic [1:0] {RCMD, RADDR, DUMMY, RDATA} read_state_t;
typedef enum logic [1:0] {WCMD, WADDR, WDATA} write_state_t;
typedef enum logic [1:0] {XCMD, XADDR, XDUMMY, XDATA} xip_state_t; 

// CSR address
parameter CYCLE_REG = 0;
parameter WDATA_REG = 4;
parameter RDATA_REG = 8;
parameter ADDR_REG  = 12;
parameter CMD_REG   = 16;
parameter CSR_REG   = 20;

// state regs
main_state_t  m_cstate, m_nstate;
read_state_t  r_cstate, r_nstate;
write_state_t w_cstate, w_nstate;
xip_state_t   x_cstate, x_nstate;

// state control signals
logic        state_read;
logic        state_write;
logic        state_xip;
logic        count_enb;
logic        count_clear;
logic        go_busy;
logic [31:0] max_count;
logic [31:0] incr_val;
logic [31:0] clk_count;
logic [31:0] cmd_cycles;
logic [31:0] addr_cycles;
logic [31:0] dummy_cycles;
logic [31:0] wdata_cycles;
logic [31:0] rdata_cycles;

// control status registers
logic [31:0] cycle_reg;
logic [31:0] wdata_reg;
logic [31:0] rdata_reg;
logic [31:0] addr_reg;
logic [7:0]  cmd_reg;
logic [31:0] control_reg;

always_ff @(posedge clk_i or negedge rst_ni) begin
    if(!rst_ni) begin
        cycle_reg   <= '0;
        wdata_reg   <= '0;
        rdata_reg   <= '0;
        addr_reg    <= '0;
        cmd_reg     <= '0;
        control_reg <= '0;
    end else begin
        if(we_i) begin
            unique case (addr_i) //csr_write
              CYCLE_REG: cycle_reg   <= wdata_i;
              WDATA_REG: wdata_reg   <= wdata_i;
              ADDR_REG : addr_reg    <= wdata_i;
              CMD_REG  : cmd_reg     <= wdata_i[7:0];
              CSR_REG  : control_reg <= wdata_i; 
              default  : begin
                            cycle_reg   <= '0;
                            wdata_reg   <= '0;
                            rdata_reg   <= '0;
                            addr_reg    <= '0;
                            cmd_reg     <= '0;
                            control_reg <= '0;
                         end
            endcase
        end
    end
    
end


always_comb begin

    // cycle reg
    cmd_cycles   = cycle_reg[5:0];
    addr_cycles  = cycle_reg[11:6];
    wdata_cycles = cycle_reg[17:12];
    rdata_cycles = cycle_reg[23:18];
    dummy_cycles = cycle_reg[29:24];

    // control reg
    state_read  = control_reg[0];
    state_write = control_reg[1];
    state_xip   = control_reg[2];
    go_busy     = control_reg[3];
end

always_ff @(posedge clk_i or negedge rst_ni) begin
    if(!rst_ni) begin
        m_nstate <= IDLE;
        r_nstate <= RCMD;
        w_nstate <= WCMD;
        x_nstate <= XCMD;
    end else begin
        m_nstate <= m_cstate;
        r_nstate <= r_cstate;
        w_nstate <= w_cstate;
        x_nstate <= x_cstate;
    end   
end

always_comb begin
   // count_enb   = '0;
   // count_clear = '0;
   // max_count   = '0;
   // incr_val    = '0;
  //  clk_count   = '0;
   // m_cstate    = IDLE;
    r_cstate    = RCMD;
    w_cstate    = WCMD;
    x_cstate    = XCMD;
    unique case (m_nstate) //main states
        IDLE: begin
                if(state_read) begin
                    m_cstate = READ;
                    max_count = cmd_cycles;
                    incr_val  = 32'b1;

                    // read required control signals 
                end else if(state_write) begin
                    m_cstate = WRITE;
                    max_count = cmd_cycles;
                    incr_val  = 32'b1;
                    
                    // write required control signals
                end else if(state_xip) begin
                    m_cstate = XIP;
                    max_count = cmd_cycles;
                    incr_val  = 32'b1;

                    // xip required control signals
                end else begin
                    m_cstate = IDLE;
                end
              end
        READ: begin
                unique case (r_nstate) //read states
                    RCMD: begin
                            if(clk_count != (cmd_cycles - 1)) begin
                                r_cstate = RCMD;
                                count_enb   = 1'b1;
                                count_clear = 1'b0;

                                // some control logic
                            end else begin 
                                r_cstate = RADDR;
                                count_clear = 1'b1;
                                count_enb   = 1'b0;
                                max_count   = addr_cycles;
                                incr_val    = 32'b1;
                                
                                // some control logic
                            end
                          end
                    RADDR: begin
                            if(clk_count != ( addr_cycles - 1)) begin
                                r_cstate = RADDR;
                                count_clear = 1'b0;
                                count_enb   = 1'b1;

                                //some control logic 
                            end else begin
                                r_cstate = DUMMY;
                                count_clear = 1'b1;
                                count_enb   = 1'b0;
                                max_count   = dummy_cycles;
                                incr_val    = 32'b1;


                                // some control logic
                            end
                           end
                    DUMMY: begin
                            if(clk_count != ( dummy_cycles - 1)) begin
                                r_cstate = DUMMY;
                                count_clear = 1'b0;
                                count_enb   = 1'b1;

                                // some control logic
                            end else begin
                                r_cstate = RDATA;
                                count_clear = 1'b1;
                                count_enb   = 1'b0;
                                max_count   = rdata_cycles;
                                incr_val    = 32'b1;

                                // some control logic
                            end
                           end
                    RDATA: begin
                            if(clk_count != ( rdata_cycles - 1)) begin
                                r_cstate = RDATA;
                                count_clear = 1'b0;
                                count_enb   = 1'b1;

                                // some control logic
                            end else begin
                                r_cstate = RCMD;
                                m_cstate = IDLE;
                                count_clear = 1'b1;
                                count_enb   = 1'b0;
                                max_count   = 32'b0;
                                incr_val    = 32'b0;
                                // some control logic 
                            end
                           end
                endcase
              end
        WRITE: begin
                unique case (w_nstate) //write states
                    WCMD: begin
                            if(clk_count != ( cmd_cycles - 1 )) begin
                                w_cstate = WCMD;
                                count_enb   = 1'b1;
                                count_clear = 1'b0;

                                // some control logic
                            end else begin
                                w_cstate = WADDR;
                                count_clear = 1'b1;
                                count_enb   = 1'b0;
                                max_count   = addr_cycles;
                                incr_val    = 1'b1;

                                // some control logic 
                            end 
                          end
                    WADDR: begin
                            if(clk_count != ( addr_cycles - 1 )) begin
                                w_cstate = WADDR;
                                count_enb   = 1'b1;
                                count_clear = 1'b0;

                                // some control logic
                            end else begin
                                w_cstate = WDATA;
                                count_clear = 1'b1;
                                count_enb   = 1'b0;
                                max_count   = wdata_cycles;
                                incr_val    = 1'b1;

                                // some control logic
                            end
                           end
                    WDATA: begin
                            if(clk_count != ( wdata_cycles - 1 )) begin
                                w_cstate = WDATA;
                                count_enb   = 1'b1;
                                count_clear = 1'b0;

                                // some control logic
                            end else begin
                                w_cstate = WCMD;
                                m_cstate = IDLE;
                                count_clear = 1'b1;
                                count_enb   = 1'b0;
                                max_count   = 32'b0;
                                incr_val    = 32'b0;

                                // some control logic
                            end
                           end
                 endcase
                end
        XIP: begin
                unique case (x_nstate) //xip states
                    XCMD: begin
                            if(clk_count != ( cmd_cycles - 1 )) begin
                                x_cstate = XCMD;
                                count_enb   = 1'b1;
                                count_clear = 1'b0;

                                // some control logic
                            end else begin
                                x_cstate = XADDR;
                                count_clear = 1'b1;
                                count_enb   = 1'b0;
                                max_count   = addr_cycles;
                                incr_val    = 1'b1;

                                // some control logic 
                            end
                          end
                    XADDR: begin
                            if(clk_count != (addr_cycles - 1 )) begin
                                x_cstate = XADDR;
                                count_enb   = 1'b1;
                                count_clear = 1'b0;
                                // some control logic
                            end else begin
                                x_cstate = XDUMMY;
                                count_clear = 1'b1;
                                count_enb   = 1'b0;
                                max_count   = dummy_cycles;
                                incr_val    = 1'b1;

                                // come control logic 
                            end
                          end
                    XDUMMY: begin
                             if(clk_count != (dummy_cycles - 1 )) begin
                                x_cstate = XDUMMY;
                                count_enb   = 1'b1;
                                count_clear = 1'b0;

                                // some control logic
                             end else begin
                                x_cstate = XDATA;
                                count_clear = 1'b1;
                                count_enb   = 1'b0;
                                max_count   = rdata_cycles;
                                incr_val    = 1'b1;

                                // some control logic
                             end
                            end
                    XDATA: begin
                            if(clk_count != (rdata_cycles - 1)) begin
                                x_cstate = XDATA;
                                count_enb   = 1'b1;
                                count_clear = 1'b0;

                                // some control logic
                            end else if(!state_xip) begin
                                x_cstate = XCMD;
                                m_cstate = IDLE;
                                count_clear = 1'b1;
                                count_enb   = 1'b0;
                                max_count   = 32'b0;
                                incr_val    = 32'b0;

                                // some control logic 
                            end else begin
                                x_cstate = XADDR;
                                count_clear = 1'b1;
                                count_enb   = 1'b0;
                                max_count   = addr_cycles;
                                incr_val    = 1'b1;

                                // some control logic
                            end
                           end
                endcase
             end
    endcase
end



clk_counter #(
    .WIDTH (32)
)u_clk_counter(
    .clk_i      (clk_i),
    .rst_ni     (rst_ni),
    .enable_i   (count_enb),
    .clear_i    (count_clear),
    .max_count_i(max_count),
    .incr_val_i (incr_val),
    .clk_count_o(clk_count)
);
endmodule