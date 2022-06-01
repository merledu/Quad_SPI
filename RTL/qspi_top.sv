module qspi_top();

	logic        inst_clk_i;
	logic        inst_rst_ni;
	logic        inst_we_i;
	logic 	     inst_re_i;
	logic [23:0] inst_addr_i;
	logic [31:0] inst_wdata_i;
	logic [31:0] inst_rdata_o;
	logic        inst_qsclk_o;
	logic        inst_qcsb_o;
	logic [3:0]  inst_qspi_i;
	logic [3:0]  inst_qspi_o;
	logic [3:0]  inst_qspi_oeb;


 qspi_core u_qspi_core(
    .clk_i(inst_clk_i),
    .rst_ni(inst_rst_ni), 
    .we_i(inst_we_i),
    .re_i(inst_re_i),
    .addr_i(inst_addr_i),
    .wdata_i(inst_wdata_i),
    .rdata_o(inst_rdata_o),
    .qsclk_o(inst_qsclk_o),
    .qcsb_o(inst_qcsb_o),
    .qspi_i(inst_qspi_i),
    .qspi_o(inst_qspi_o),
    .qspi_oeb(inst_qspi_oeb)
  );

  initial begin
    inst_clk_i = 0 ; inst_rst_ni = 0;
#10 inst_rst_ni = 1; inst_we_i = 1; inst_addr_i = 0; inst_wdata_i = 32'hF208182;
#10 inst_addr_i = 24'd20; inst_wdata_i = 32'h4;

  end

	always #5 inst_clk_i = ~inst_clk_i;

endmodule