module qspi_tx_tb_top ();
  logic        inst_clk_i;
  logic        inst_rst_ni;
  logic        inst_msb;
  logic        inst_lsb;
  logic [31:0] inst_data_in;
  logic        inst_ready;
  logic [3:0]  inst_qsd_o;


  qspi_tx_shift u_qspi_tx_shift(
  clk_i(inst_clk_i),
  rst_ni(inst_rst_ni),
  msb(inst_msb),
  lsb(inst_lsb),
  data_in(inst_data_in),
  ready(inst_ready),
  qsd_o(inst_qsd_o)
);

    initial begin
        inst_clk_i = 0; inst_rst_ni = 0;
    #10 inst_rst_ni = 1 ; inst_ready = 1; inst_lsb = 1; inst_data_in; inst_msb = 1;
  end
  	always #5 inst_clk_i = ~inst_clk_i;


endmodule