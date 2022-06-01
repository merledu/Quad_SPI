module clk_counter #(
    parameter WIDTH = 32
)(
    input logic clk_i,
    input logic rst_ni,

    input logic enable_i,
    input logic clear_i,
    input logic [WIDTH-1:0] max_count_i,
    input logic [WIDTH-1:0]  incr_val_i,

    output logic [WIDTH-1:0] clk_count_o
);

    always_ff @(posedge clk_i or negedge rst_ni) begin
        if(!rst_ni) begin
            clk_count_o <= '0;
        end else begin
            if(enable_i && ~(clk_count_o == max_count_i)) begin
                clk_count_o <= clk_count_o + incr_val_i;
            end else if (enable_i && (clk_count_o == max_count_i)) begin
                clk_count_o <= clk_count_o;
            end else if (clear_i) begin
                clk_count_o <= '0;
            end
        end
    end
endmodule