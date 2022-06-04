module qspi_transmitter (

    input logic clk_i,
    input logic rst_ni,
    
    input logic [31:0] p_data_i,
    input logic        t_enb_i,
    input logic        t_load_i,
    input logic        lsb_i,
    output logic [3:0] s_out_o,
    output logic [3:0] s_oeb_o
);

    logic [3:0]  shift_out;
    logic [31:0] temp_reg;
    logic [3:0]  s_oeb;
    always_ff @(negedge clk_i or negedge rst_ni) begin
        if(!rst_ni) begin
            s_oeb_o   <= 4'b1111;
            s_oeb     <= 4'b1111;
            temp_reg  <= '0;
            s_out_o   <= '0;
        end else begin
            s_out_o <= shift_out;
            if(t_load_i) begin
                temp_reg <= p_data_i;
            end else if(t_enb_i) begin
                if(lsb_i) begin
                    temp_reg     <= {4'b0, temp_reg[31:4]};
                end else begin
                     temp_reg     <= {temp_reg[27:0], 4'b0};
                end
            end
        end
    end
    
    always_comb begin
        if(lsb_i) begin
            shift_out[0] = temp_reg[0];
            shift_out[1] = temp_reg[1];
            shift_out[2] = temp_reg[2];
            shift_out[3] = temp_reg[3];
        end else begin
            shift_out[0] = temp_reg[31];
            shift_out[1] = temp_reg[30];
            shift_out[2] = temp_reg[29];
            shift_out[3] = temp_reg[28];
        end
    end

    //assign s_out_o = shift_out;
endmodule