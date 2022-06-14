module qspi_core #(
    parameter A_WIDTH = 24,
    parameter D_WIDTH = 32
)(
    input logic clk_i,
    input logic rst_ni,
// Bus interface 
    input  logic               we_i,
    input  logic               re_i,






// QSPI interface
    output logic       qsclk_o,
    output logic       qcsb_o,
    input  logic [3:0] qspi_i,
    output logic [3:0] qspi_o,
    output logic [3:0] qspi_oeb,
    
    output logic       intr_tx,
    output logic       intr_rx,
    output logic       xip_valid
);

    logic [A_WIDTH-1:0] addr_i;
    logic [D_WIDTH-1:0] wdata_i;
    logic [D_WIDTH-1:0] rdata_o;

// qspi states
typedef enum logic [1:0] {IDLE, READ, WRITE, XIP} main_state_t;
typedef enum logic [1:0] {RCMD, RADDR, DUMMY, RDATA} read_state_t;
typedef enum logic [1:0] {WCMD, WADDR, WDATA} write_state_t;
typedef enum logic [1:0] {XCMD, XADDR, XDUMMY, XDATA} xip_state_t; 

// CSR address
localparam CYCLE_REG = 0;
localparam WDATA_REG = 4;
localparam RDATA_REG = 8;
localparam ADDR_REG  = 12;
localparam CMD_REG   = 16;
localparam CSR_REG   = 20;

// monitor signals 

logic transmit_intr, transmit_intr_fp;
logic receive_intr, receive_intr_fp;
logic valid_xip_data, valid_xip_data_fp;
logic tx_clear;
logic rx_clear;

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
logic        enable_xip_cmd;
logic        read_dummy;
logic        dummy_cs_high;
logic        read_reg_cmd;
logic        read_clear;
logic        write_clear;
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

// transmitter control signals
logic [31:0] t_data;
logic        t_load;
logic        t_enb;
logic        t_lsb;
logic        t_load_latch;
logic        load;


// receiver control signals

logic r_enb;
logic r_enb_reg1;
logic r_enb_reg2;
logic r_enb_latch;
logic r_enb_t;
logic r_lsb;


// qspi clock control signals
logic qclk_enb, qclk_reg;
logic qclk_out_reg;
logic qclk_internal;
logic qclk_latch_intrnal;
logic clk_latch;

// slave select control signal
logic qcsb_enb, qcsb_latch, qcsb_reg;
logic qcbs_out_reg;

// output enable signal;
logic [3:0] qspi_oe;
logic [3:0] oeb_latch;
logic [3:0] oeb_reg;


// clock control logic 
/*
always_comb begin
if(!clk_i) begin
    clk_latch = qclk_reg;
    qclk_latch_intrnal = qclk_enb;
    t_load_latch = t_load;
    r_enb_latch = r_enb;
end
*/

always @(negedge clk_i) begin
clk_latch <= qclk_reg;
qclk_latch_intrnal <= qclk_enb;
t_load_latch <= t_load;
r_enb_latch <= r_enb;
end

assign load    = t_load_latch  && clk_i;
assign qsclk_o = clk_latch && clk_i;
assign qclk_internal = qclk_latch_intrnal && clk_i; // not in use
assign r_enb_t = r_enb_latch;// && clk_i;

// slave select latching logic

always @(negedge clk_i) begin
if(!rst_ni) begin
qcsb_latch <= '0;
oeb_latch <= '0;
end
else begin
qcsb_latch <= qcsb_enb;
oeb_latch  <= qspi_oe;
end
end

/*
always_comb begin
if(!clk_i) begin
  qcsb_latch = qcsb_enb;
  oeb_latch  = qspi_oe;
end
end
*/
///*
always_ff @(negedge clk_i or negedge rst_ni) begin
	if(!rst_ni) begin
		qcsb_o      <= 1'b1;
        qcsb_reg    <= 1'b1;
        qcbs_out_reg <= 1'b1;
        qspi_oeb     <= 4'b0000;
        oeb_reg      <= 4'b0000;
	end else begin
        qcsb_reg <= qcsb_latch && qcsb_enb;
		qcbs_out_reg   <= qcsb_reg;
		qcsb_o   <= qcbs_out_reg;
		oeb_reg  <= oeb_latch;
		qspi_oeb <= oeb_reg;
		
	end
end
//*/
always_ff @(posedge clk_i or negedge rst_ni) begin
    if(!rst_ni) begin
        cycle_reg   <= '0;
        wdata_reg   <= '0;
        rdata_reg   <= '0;
        addr_reg    <= '0;
        cmd_reg     <= '0;
        control_reg <= '0;
	    qclk_reg    <= '0;
	    qclk_out_reg <= '0;
	    r_enb_reg1   <= '0;
	    r_enb_reg2   <= '0;
	    intr_tx      <= 1'b0;
	    intr_rx      <= 1'b0;
	    xip_valid    <= 1'b0;
	    receive_intr_fp <= 1'b0;
        transmit_intr_fp <= 1'b0;
        valid_xip_data_fp <= 1'b0;
    end else begin
    
        if(rx_clear) begin
            receive_intr_fp <= '0;
            intr_rx         <= '0;
        end else begin
            receive_intr_fp <= receive_intr;
            intr_rx         <= receive_intr_fp;
        end
        
        if(tx_clear) begin
            transmit_intr_fp <= '0;
            intr_tx          <= '0;
        end else begin
            transmit_intr_fp <= transmit_intr;
            intr_tx          <= transmit_intr_fp;
        end
        
        valid_xip_data_fp <= valid_xip_data;
        xip_valid         <= valid_xip_data_fp;
        
        r_enb_reg1   <= r_enb;
        r_enb_reg2   <= r_enb_reg1;
        qclk_out_reg <= qclk_enb;
        qclk_reg     <= qclk_out_reg;
	
        if(we_i) begin
            unique case (addr_i) //crs_write
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
        end else begin
            control_reg[10] <= 1'b1;
            control_reg[11] <= 1'b1;
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
    state_read     = control_reg[0];
    state_write    = control_reg[1];
    state_xip      = control_reg[2];
    go_busy        = control_reg[3];
    enable_xip_cmd = control_reg[4];
    read_dummy     = control_reg[5];
    dummy_cs_high  = control_reg[6];
    read_reg_cmd   = control_reg[7];
    t_lsb          = control_reg[8];
    r_lsb          = control_reg[9];
    read_clear     = control_reg[10];
    write_clear    = control_reg[11];
    tx_clear       = control_reg[12];
    rx_clear       = control_reg[13];
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
  // m_cstate = IDLE;
   r_cstate = RCMD;
   w_cstate = WCMD;
 if(enable_xip_cmd) begin
   x_cstate = XCMD;
 end else begin
   x_cstate = XADDR;
 end
t_enb = '0;
t_load = '0;
r_enb = '0;
//m_cstate = IDLE;
    unique case (m_nstate) // main_states
        IDLE: begin
                if(state_read && ~read_clear) begin
                    m_cstate  = READ;
                    max_count = cmd_cycles;
                    incr_val  = 32'b1;
                    qclk_enb  = 1'b1;
                    qcsb_enb  = 1'b0;
                    qspi_oe   = 4'b000; 
                    transmit_intr = 1'b0;
                    receive_intr = 1'b0;
                    valid_xip_data = 1'b0;
                    count_enb = 1'b0;
                    count_clear = 1'b0;
                    if(t_lsb) begin
                        t_data    = {24'b0, cmd_reg};
                    end else begin
                        t_data    = {cmd_reg, 24'b0};
                    end

                    t_enb       = 1'b1;
                    

                    // read required control signals 
                end else if(state_write && ~write_clear) begin
                    m_cstate = WRITE;
                    max_count = cmd_cycles;
                    incr_val  = 32'b1;
                    qclk_enb  = 1'b1;
                    qcsb_enb  = 1'b0; 
                    qspi_oe   = 4'b000;
                    transmit_intr = 1'b0;
                    receive_intr = 1'b0;
                    valid_xip_data = 1'b0;
                    count_enb = 1'b0;
                    count_clear = 1'b0;
                    
                    if(t_lsb) begin
                        t_data    = {24'b0, cmd_reg};
                    end else begin
                        t_data    = {cmd_reg, 24'b0};
                    end
                    t_enb       = 1'b1;
                    
                    
                    // write required control signals
                end else if(state_xip) begin
                    m_cstate = XIP;
                   if(enable_xip_cmd) begin
                    max_count = cmd_cycles;
                    incr_val  = 32'b1;
                    qclk_enb  = 1'b1;
                    qcsb_enb  = 1'b0;
                    qspi_oe   = 4'b000;
                    transmit_intr = 1'b0;
                    receive_intr = 1'b0;
                    valid_xip_data = 1'b0;
                    count_enb = 1'b0;
                    count_clear = 1'b0;
                    if(t_lsb) begin
                        t_data    = {24'b0, cmd_reg};
                    end else begin
                        t_data    = {cmd_reg, 24'b0};
                    end
                    t_load    = 1'b0;
                    t_enb       = 1'b1;
                    
                   end else begin
                    max_count = addr_cycles ;
                    incr_val  = 32'b1;
                    qclk_enb  = 1'b1;
                    qcsb_enb  = 1'b0;
                    qspi_oe   = 4'b000;
                    transmit_intr = 1'b0;
                    receive_intr = 1'b0;
                    valid_xip_data = 1'b0;
                    count_enb = 1'b0;
                    count_clear = 1'b0;
                    if(t_lsb) begin
                        t_data    = {8'b0, addr_i[23:0]};
                    end else begin
                        t_data    = {addr_i[23:0], 8'b0};
                    end
                    t_enb     = 1'b1;
                    
                   end


                    // xip required control signals
                end else begin
                    m_cstate = IDLE;
                    qclk_enb  = 1'b0;
                    max_count = 32'b0;
                    incr_val  = 32'b0;
                    count_enb   = 1'b0;
                    count_clear = 1'b1;
                    qcsb_enb    = 1'b1;
                    t_load      = 1'b0;
                    t_enb       = 1'b0;
                    r_enb       = 1'b0;
                    receive_intr = 1'b0;
                    transmit_intr = 1'b0;
                    valid_xip_data = 1'b0;
                    qspi_oe     = 4'b0000;
                    count_enb = 1'b0;
                    count_clear = 1'b0;
                end
              end
        READ: begin
                unique case (r_nstate) // read_states
                    RCMD: begin
                            if(clk_count != (cmd_cycles-1)) begin
                                m_cstate = READ;
                                r_cstate = RCMD;
                                count_enb   = 1'b1;
                                count_clear = 1'b0;
                                max_count = cmd_cycles;
                                incr_val = 32'b1;
		                  		      qclk_enb = 1'b1;
				                        qcsb_enb = 1'b0;
				                        qspi_oe = 4'b0000;
				                        transmit_intr = 1'b0;
				                        receive_intr = 1'b0;
				                        valid_xip_data = 1'b0;
				                        count_clear = 1'b0;
                                if(clk_count >= 1) begin
                                    t_load    = 1'b0;
                                end else begin
                                    t_load    = 1'b1;
                                end
                                // some control logic
                            end else begin 
                                if(read_reg_cmd) begin 
                                    m_cstate = READ;
                                    r_cstate = RDATA;
				                            qclk_enb = 1'b1;
				                            qcsb_enb = 1'b0;
				                            qspi_oe = 4'b0000;
                                    count_clear = 1'b1;
                                    count_enb   = 1'b0;
                                    max_count   = rdata_cycles;
                                    incr_val    = 32'b1;
                                    t_load    = 1'b0;
                                    qspi_oe   = 4'b1111;
                                    transmit_intr = 1'b0;
                                    receive_intr = 1'b0;
                                    valid_xip_data = 1'b0;
                                end else begin
                                    m_cstate = READ;
                                    r_cstate = RADDR;
                                    count_clear = 1'b1;
                                    count_enb   = 1'b0;
		                  		          qclk_enb = 1'b1;
			                 	            qcsb_enb = 1'b0;
                				            qspi_oe = 4'b0000;
                                    max_count   = addr_cycles -1;
                                    incr_val    = 32'b1;
                                    transmit_intr = 1'b0;
                                    receive_intr = 1'b0;
                                    valid_xip_data = 1'b0;
                                    if(t_lsb) begin
                                        t_data    = {8'b0,addr_reg};
                                    end else begin
                                        t_data    = {addr_reg, 8'b0};
                                    end
                                    
                                    t_load    = 1'b0;
                                end
                                
                                
                                // some control logic
                            end
                          end
                    RADDR: begin
                            if(clk_count != (addr_cycles-2)) begin
                                m_cstate = READ;
                                r_cstate = RADDR;
                                count_clear = 1'b0;
			                 	        qclk_enb = 1'b1;
                				        qcsb_enb = 1'b0;
                				        qspi_oe = 4'b0000;
                                count_enb   = 1'b1;
                                transmit_intr = 1'b0;
                                receive_intr = 1'b0;
                                valid_xip_data = 1'b0;
                                max_count = addr_cycles;
                                incr_val = 32'b1;
                                if(clk_count >= 1) begin
                                    t_load    = 1'b0;
                                end else begin
                                    t_load    = 1'b1;
                                end
                                //some control logic 
                            end else if(read_dummy) begin
                                m_cstate = READ;
                                r_cstate = DUMMY;
                                count_clear = 1'b1;
                                count_enb   = 1'b0;
                			        	qclk_enb = 1'b1;
                			        	qcsb_enb = 1'b0;
                			        	qspi_oe = 4'b0000;
                                max_count   = dummy_cycles;
                                incr_val    = 32'b1;
                                transmit_intr = 1'b0;
                                receive_intr = 1'b0;
                                valid_xip_data = 1'b0;
                                if(dummy_cs_high) begin
                                  qcsb_enb  = 1'b1;
                                end else begin
                                  qcsb_enb  = 1'b0;
                                end
                                // some control logic
                            end else begin
                                m_cstate = READ;
                                r_cstate = RDATA;
                                count_clear = 1'b1;
                                count_enb   = 1'b0;
				                        qclk_enb = 1'b1;
				                        qcsb_enb = 1'b0;
				                        qspi_oe = 4'b0000;
                                max_count   = rdata_cycles;
                                incr_val    = 32'b1;
                                transmit_intr = 1'b0;
                                receive_intr = 1'b0;
                                valid_xip_data = 1'b0;
                            end
                           end
                    DUMMY: begin
                            if(clk_count != (dummy_cycles-1)) begin
                                m_cstate = READ;
                                r_cstate = DUMMY;
                                count_clear = 1'b0;
                                count_enb   = 1'b1;
			                        	qspi_oe   = 4'b1111;
			                        	qclk_enb = 1'b0;
			                        	qcsb_enb = 1'b0;  
                                transmit_intr = 1'b0;
                                receive_intr = 1'b0;
                                valid_xip_data = 1'b0;
                                max_count = dummy_cycles;
                                incr_val = 32'b1;
                                // some control logic
                            end else begin
                                m_cstate = READ;
                                r_cstate = RDATA;
                                count_clear = 1'b1;
			                        	qclk_enb = 1'b1;
			                        	qcsb_enb = 1'b0;
                                count_enb   = 1'b0;
                                max_count   = rdata_cycles;
                                incr_val    = 32'b1;
                                t_enb       = 1'b0;
                                qspi_oe   = 4'b1111;
                                transmit_intr = 1'b0;
                                receive_intr = 1'b0;
                                valid_xip_data = 1'b0;
                                // some control logic
                            end
                           end
                    RDATA: begin
                            if(clk_count != (rdata_cycles)) begin
                                m_cstate = READ;
                                r_cstate = RDATA;
                                count_clear = 1'b0;
			                        	qclk_enb = 1'b1;
			                        	qspi_oe = 4'b0000;
                                count_enb   = 1'b1;
                                qcsb_enb    = 1'b0;
                                qspi_oe   = 4'b1111;
                                r_enb     = 1'b1;
                                transmit_intr = 1'b0;
                                receive_intr = 1'b0;
                                valid_xip_data = 1'b0;
                                max_count = rdata_cycles;
                                incr_val = 32'b1;
                                // some control logic
                            end else begin
                                r_cstate = RCMD;
                                m_cstate = IDLE;
                                count_clear = 1'b1;
                                count_enb   = 1'b0;
			                        	qclk_enb = 1'b0;
			                        	valid_xip_data = 1'b0;
                                max_count   = 32'b0;
                                incr_val    = 32'b0;
                                qclk_enb    = 1'b0;
                                qcsb_enb  = 1'b1;
                                qspi_oe   = 4'b0000;
                                r_enb     = 1'b0;
                                receive_intr = 1'b1;
                                transmit_intr = 1'b0;
                                receive_intr = 1'b0;
                                // some control logic 
                            end
                           end
                           
                           
                endcase
              end
        WRITE: begin
                unique case (w_nstate) // write_state
                    WCMD: begin
                            if(clk_count != (cmd_cycles -1)) begin
                                m_cstate = WRITE;
                                w_cstate = WCMD;
                                count_enb   = 1'b1;
			                        	qclk_enb = 1'b1;
			                        	qcsb_enb = 1'b0;
			                        	qspi_oe = 4'b0000;
			                        	receive_intr = 1'b0;
			                        	valid_xip_data = 1'b0;
			                        	max_count = cmd_cycles;
			                        	incr_val = 32'b1;
                                count_clear = 1'b0;
                                transmit_intr = 1'b0;
                                if(clk_count >= 1) begin
                                    t_load    = 1'b0;
                                end else begin
                                    t_load    = 1'b1;
                                end;

                                // some control logic
                            end else begin
                                m_cstate = WRITE;
                                w_cstate = WADDR;
                                count_clear = 1'b1;
			                        	qclk_enb = 1'b1;
			                        	qcsb_enb = 1'b0;
			                        	qspi_oe = 4'b0000;
			                        	receive_intr = 1'b0;
			                        	valid_xip_data = 1'b0;
                                count_enb   = 1'b0;
                                max_count   = addr_cycles;
                                incr_val    = 1'b1;
                                transmit_intr = 1'b0;
                                if(t_lsb) begin
                                    t_data      = {8'b0, addr_reg[23:0]};
                                end else begin
                                    t_data      = {addr_reg[23:0], 8'b0};
                                end
                                t_load      = 1'b0;
                                // some control logic 
                            end 
                          end
                    WADDR: begin
                            if(clk_count != (addr_cycles -1)) begin
                                m_cstate = WRITE;
                                w_cstate = WADDR;
                                count_enb   = 1'b1;
                                count_clear = 1'b0;
			                        	qclk_enb = 1'b1;
			                        	qcsb_enb = 1'b0;
			                        	qspi_oe = 4'b0000;
			                        	transmit_intr = 1'b0;
			                        	receive_intr = 1'b0;
			                        	valid_xip_data = 1'b0;
			                        	max_count = addr_cycles;
			                        	incr_val = 32'b1;
                                if(clk_count >= 1) begin
                                    t_load    = 1'b0;
                                end else begin
                                    t_load    = 1'b1;
                                end

                                // some control logic
                            end else begin
                                m_cstate = WRITE;
                                w_cstate = WDATA;
                                count_clear = 1'b1;
                                count_enb   = 1'b0;
			                        	qclk_enb = 1'b1;
			                        	qcsb_enb = 1'b0;
			                        	qspi_oe = 4'b0000;
			                        	receive_intr = 1'b0;
			                        	valid_xip_data = 1'b0;
                                max_count   = wdata_cycles;
                                incr_val    = 1'b1;
                                t_data      = wdata_reg;
                                t_load      = 1'b0;
                                transmit_intr = 1'b0;
                                // some control logic
                            end
                           end
                    WDATA: begin
                            if(clk_count != (wdata_cycles -1)) begin
                                m_cstate = WRITE;
                                w_cstate = WDATA;
                                count_enb   = 1'b1;
                                count_clear = 1'b0;
			                        	qclk_enb = 1'b1;
			                        	qcsb_enb = 1'b0;
			                        	qspi_oe = 4'b0000;
			                        	transmit_intr = 1'b0;
			                        	receive_intr = 1'b0;
			                        	valid_xip_data = 1'b0;
			                        	max_count = wdata_cycles;
			                        	incr_val = 32'b1;
                                if(clk_count >= 1) begin
                                    t_load    = 1'b0;
                                end else begin
                                    t_load    = 1'b1;
                                end

                                // some control logic
                            end else begin
                                w_cstate = WCMD;
                                m_cstate = IDLE;
                                count_clear = 1'b1;
                                count_enb   = 1'b0;
			                        	qclk_enb = 1'b0;
			                        	receive_intr = 1'b0;
			                        	valid_xip_data = 1'b0;
                                max_count   = 32'b0;
                                incr_val    = 32'b0;
                                qclk_enb    = 1'b0;
                                qcsb_enb  = 1'b1;
                                transmit_intr = 1'b1;
                                qspi_oe = 4'b0000;
                
                                // some control logic
                            end
                           end
                 endcase
                end
        XIP: begin
                unique case (x_nstate) // xip_state
                    XCMD: begin
                            if(clk_count != (cmd_cycles-1 )) begin
                                m_cstate = XIP;
                                x_cstate = XCMD;
                                count_enb   = 1'b1;
                                count_clear = 1'b0;
				                        qclk_enb = 1'b1;
				                        qcsb_enb = 1'b0;
				                        qspi_oe = 4'b0000;
				                        transmit_intr = 1'b0;
				                        receive_intr = 1'b0;
				                        valid_xip_data = 1'b0;
				                        max_count = cmd_cycles;
				                        incr_val = 32'b1;
                                if(clk_count >= 1) begin
                                    t_load    = 1'b0;
                                end else begin
                                    t_load    = 1'b1;
                                end
                                valid_xip_data = 1'b0;

                                // some control logic
                            end else begin
                                m_cstate = XIP;
                                x_cstate = XADDR;
                                count_clear = 1'b1;
                                count_enb   = 1'b0;
			                        	qclk_enb = 1'b1;
			                        	qcsb_enb = 1'b0;
			                        	qspi_oe = 4'b0000;
			                        	transmit_intr = 1'b0;
			                        	receive_intr = 1'b0;
			                        	valid_xip_data = 1'b0;
                                max_count   = addr_cycles -1;
                                incr_val    = 1'b1;
                                if(t_lsb) begin
                                   t_data    = {8'b0, addr_i[23:0]};
                                end else begin
                                   t_data    = {addr_i[23:0], 8'b0};
                                end
                                t_load      = 1'b0;

                                // some control logic 
                            end
                          end
                    XADDR: begin
                            if(clk_count != (addr_cycles-2)) begin
                                m_cstate = XIP;
                                x_cstate = XADDR;
                                count_enb   = 1'b1;
			                        	qclk_enb = 1'b1;
			                        	qcsb_enb = 1'b0;
			                        	qspi_oe = 4'b0000;
			                        	transmit_intr = 1'b0;
			                        	receive_intr = 1'b0;
			                        	valid_xip_data = 1'b0;
			                        	max_count = addr_cycles;
			                        	incr_val = 32'b1;
                                count_clear = 1'b0;
                                if(clk_count >= 1) begin
                                    t_load    = 1'b0;
                                end else begin
                                    t_load    = 1'b1;
                                end
                                valid_xip_data = 1'b0;

                                // some control logic
                            end else begin
                                m_cstate = XIP;
                                x_cstate = XDUMMY;
                                count_clear = 1'b1;
                                count_enb   = 1'b0;
			                        	qclk_enb = 1'b1;
			                        	qcsb_enb = 1'b0;
			                        	qspi_oe = 4'b0000;
			                        	transmit_intr = 1'b0;
			                        	receive_intr = 1'b0;
			                        	valid_xip_data = 1'b0;
                                max_count   = dummy_cycles;
                                incr_val    = 1'b1;
                                if(dummy_cs_high) begin
                                  qcsb_enb  = 1'b1;
                                end else begin
                                  qcsb_enb  = 1'b0;
                                end
                                t_load    = 1'b0;
                                
                                // come control logic 
                            end
                          end
                    XDUMMY: begin
                             if(clk_count != (dummy_cycles -1 )) begin
                                m_cstate = XIP;
                                x_cstate = XDUMMY;
                                count_enb   = 1'b1;
                                count_clear = 1'b0;
			                        	qclk_enb = 1'b1;
			                        	qcsb_enb = 1'b0;
			                        	qspi_oe = 4'b0000;
			                        	transmit_intr = 1'b0;
			                        	receive_intr = 1'b0;
			                        	valid_xip_data = 1'b0;
			                        	max_count = dummy_cycles;
			                        	incr_val = 32'b1;
				                        qspi_oe     = 4'b1111;

                                // some control logic
                             end else begin
                                m_cstate = XIP;
                                x_cstate = XDATA;
                                count_clear = 1'b1;
                                count_enb   = 1'b0;
			                        	qclk_enb = 1'b0;
			                        	qcsb_enb = 1'b0;
			                        	qspi_oe = 4'b0000;
			                        	transmit_intr = 1'b0;
			                        	receive_intr = 1'b0;
			                        	valid_xip_data = 1'b0;
                                max_count   = rdata_cycles;
                                incr_val    = 1'b1;
                                t_enb     = 1'b0;
                                
                                // some control logic
                             end
                            end
                    XDATA: begin
                            if(clk_count != (rdata_cycles)) begin
                                m_cstate = XIP;
                                x_cstate = XDATA;
                                count_enb   = 1'b1;
                                count_clear = 1'b0;
			                        	qclk_enb = 1'b0;
			                        	transmit_intr = 1'b0;
			                        	receive_intr = 1'b0;
			                        	valid_xip_data = 1'b0;
			                        	max_count = rdata_cycles;
			                        	incr_val = 32'b1;
                                qcsb_enb  = 1'b0;
                                r_enb      = 1'b1;
			                        	qspi_oe = 4'b0000;

                                // some control logic
                            end else begin
                             if(!state_xip) begin
                                x_cstate = XCMD;
                                m_cstate = IDLE;
                                count_clear = 1'b1;
                                count_enb   = 1'b0;
			                        	qclk_enb = 1'b0;
			                        	receive_intr = 1'b0;
			                        	valid_xip_data = 1'b0;
                                max_count   = 32'b0;
                                incr_val    = 32'b0;
                                qclk_enb    = 1'b0;
                                qcsb_enb    = 1'b1;
                                r_enb       = 1'b0;
                                valid_xip_data = 1'b1;
                                transmit_intr = 1'b1;
			                        	qspi_oe = 4'b0000;

                                // some control logic 
                             end else begin
                                m_cstate = XIP;
                                x_cstate = XADDR;
                                count_clear = 1'b1;
                                count_enb   = 1'b0;
			                        	qclk_enb = 1'b0;
			                        	qcsb_enb = 1'b0;
			                        	receive_intr = 1'b0;
                                max_count   = addr_cycles -1;
                                incr_val    = 1'b1;
				                        qspi_oe = 4'b0000;
				                        transmit_intr = 1'b0;
				                        valid_xip_data = 1'b0;
                                if(t_lsb) begin
                                   t_data    = {8'b0, addr_i[23:0]};
                                end else begin
                                   t_data    = {addr_i[23:0], 8'b0};
                                end
                                t_load      = 1'b0;
                                t_enb       = 1'b1;
                                qspi_oe     = 4'b0000;
                                r_enb      = 1'b0;
                                valid_xip_data = 1'b1;
                                // some control logic
                             end
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


qspi_transmitter u_transmitter(

    .clk_i      (clk_i),
    .rst_ni     (rst_ni),
    .p_data_i   (t_data),
    .t_enb_i    ((state_read || state_xip) ? t_enb : (t_enb || ~qcsb_o)),
    .t_load_i   (t_load),
    .lsb_i      (t_lsb),
    .s_out_o    (qspi_o)
);

qspi_receiver u_receiver(
    .clk_i  (clk_i),
    .rst_ni (rst_ni),
    .s_in_i (qspi_i),
    .enb_i  (r_enb_reg2),
    .lsb_i  (r_lsb),
    .p_out_o(rdata_o)
);
endmodule

