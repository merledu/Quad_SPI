module qspi_rx_tb_top();
  logic        inst_clk_i;
  logic        inst_rst_ni;
  logic [3:0]  inst_qsd_i;
  logic        inst_lsb;
  logic        inst_msb;
  logic        inst_valid;
  logic [31:0] inst_data_o;

 qspi_rx_shift u_qspi_rx_shift(
  .clk_i(inst_clk_i),
  .rst_ni(inst_rst_ni),
  .qsd_i(inst_qsd_i),
  .lsb(inst_lsb),
  .msb(inst_msb),
  .valid(inst_valid),
  .data_o(inst_data_o)
);

  initial begin
        inst_clk_i = 0; inst_rst_ni = 0;
    #10 inst_rst_ni = 1 ; inst_valid = 1; inst_lsb = 0; inst_qsd_i = 4'd1, inst_msb = 1;
    #10 inst_qsd_i = 4'd2;
    #10 inst_qsd_i = 4'd3;
    #10 inst_qsd_i = 4'd4;
    #10 inst_qsd_i = 4'd5;
    #10 inst_qsd_i = 4'd6;
    #10 inst_qsd_i = 4'd7;
    #10 inst_qsd_i = 4'd8;
  end
  	always #5 inst_clk_i = ~inst_clk_i;


endmodule