module qspi_rx_shift(
  input  logic        clk_i,
  input  logic        rst_ni,
  input  logic [3:0]  qsd_i,
  input  logic        lsb,
  input  logic        msb,
  input  logic        valid,
  output logic [31:0] data_o

);

  logic [31:0] data_reg;

  always @(posedge clk_i or negedge rst_ni) begin
    if(!rst_ni) begin
      data_reg <= '0;
    end
    else if (valid) begin
      if(lsb) begin
        data_reg  <= {data_reg[27:0], qsd_i};
      end
      else if (msb) begin
        data_reg <= {qsd_i, data_reg[31:4]};
      end
    end
  end

  assign data_o = data_reg;



endmodule