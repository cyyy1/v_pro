//------------------------------------------------------------------------------------------------------------------------//
//2017-08-13: V0.1 zhangshi   Original version. 
//2017-11-09: V0.2 zhangshi   R&W register's reserved bit can't be wrote.
//2020-06-29: V0.3 chenyi	  update register config using apb interface. 
//------------------------------------------------------------------------------------------------------------------------//
 
   `include "param_def.v"
   module ctrl_regs(	clk_i,
						rstn_i,
						//apb_intface
						paddr_i,
						pwr_i,
						pen_i,
						psel_i,
						pwdata_i,
						
						prdata_o,
						pready_o,
						pslverr_o,
						
						slv0_pkglen_o,
						slv1_pkglen_o,
						slv2_pkglen_o,
						
						slv0_prio_o,
						slv1_prio_o,
						slv2_prio_o,
						
						slv0_margin_i,
						slv1_margin_i,
						slv2_margin_i,
			
					 	slv0_en_o,
						slv1_en_o,
						slv2_en_o);
						
input 							clk_i;
input 							rstn_i;
input [`ADDR_WIDTH-1:0] 		paddr_i;
input 							pwr_i;
input							pen_i;
input							psel_i;
input	[`CMD_DATA_WIDTH-1:0]	pwdata_i;
output  [`CMD_DATA_WIDTH-1:0] 	prdata_o;
output 							pready_o;
output							pslverr_o;

input [`FIFO_MARGIN_WIDTH-1:0]  slv0_margin_i;
input [`FIFO_MARGIN_WIDTH-1:0]  slv1_margin_i;
input [`FIFO_MARGIN_WIDTH-1:0]  slv2_margin_i;

parameter [1:0]     		    st_IDLE  =2'b00 ;
parameter [1:0]        		    st_SETUP =2'b01 ;
parameter [1:0]				    st_ACC   =2'b10 ;

reg [1:0] 						last_st, cur_st ;
reg [`CMD_DATA_WIDTH-1:0]      	ctrl_mem [2:0]; 
reg [`CMD_DATA_WIDTH-1:0]      	ro_mem   [2:0];
reg [`CMD_DATA_WIDTH-1:0] 		data_rd_r;
reg	[`ADDR_WIDTH-1:0]       	addr_r;

output  [`PAC_LEN_WIDTH-1:0]  	slv0_pkglen_o;
output  [`PAC_LEN_WIDTH-1:0]  	slv1_pkglen_o;
output  [`PAC_LEN_WIDTH-1:0]  	slv2_pkglen_o;
output  [`PRIO_WIDTH-1:0]		slv0_prio_o;
output  [`PRIO_WIDTH-1:0]  		slv1_prio_o;
output  [`PRIO_WIDTH-1:0]  		slv2_prio_o;
output   						slv0_en_o;
output   						slv1_en_o;
output   						slv2_en_o;

wire                			is_ctrl_rng_s;
wire                			is_ro_rng_s;
wire                			is_err_rng_s;


wire                			is_st_idle_s, is_st_setup_s, is_st_acc_s; 

wire                			is_addr_freeslot_0_s;
wire                			is_addr_freeslot_1_s;
wire                			is_addr_freeslot_2_s;

wire                			idx_0_s;
wire                			idx_1_s;
wire                			idx_2_s;

wire                			is_addr_chnl0_s;
wire                			is_addr_chnl1_s;
wire                			is_addr_chnl2_s;


//--------------------------------------------------------------------------------------------------------
// State Passing
//--------------------------------------------------------------------------------------------------------
always @(posedge clk_i or negedge rstn_i)
begin
    if (!rstn_i) last_st <= st_IDLE   ;
    else          last_st <= cur_st    ;
end


//--------------------------------------------------------------------------------------------------------
// State Transition 
//--------------------------------------------------------------------------------------------------------
always @(*) begin
    case (last_st)
        st_IDLE    : if (psel_i) 
                         cur_st <= st_SETUP   ;
                     else 
                         cur_st <= st_IDLE;

        st_SETUP   : cur_st <= st_ACC  ;  // PSEL=1 at this phase and goto st_ACC unconditionally

        st_ACC     : 
                     if (psel_i && pen_i) begin
                         cur_st <= st_ACC ;
                     end else begin
                         cur_st <= st_IDLE;
                     end 
    endcase
end 

assign is_st_idle_s  = (cur_st == st_IDLE ) ? 1'b1 : 1'b0 ;
assign is_st_setup_s = (cur_st == st_SETUP) ? 1'b1 : 1'b0 ;
assign is_st_acc_s   = (cur_st == st_ACC  ) ? 1'b1 : 1'b0 ;


always @(posedge clk_i or negedge rstn_i)
begin
    if (!rstn_i) begin
        addr_r <= 0 ;
    end else begin
        if (is_st_setup_s)  begin
            addr_r <= paddr_i ;
        end 
    end
end 

always @(*) begin
  data_rd_r <= 0; 
  if (is_st_acc_s) begin
      if (~pwr_i) begin 
          if (is_addr_chnl0_s)  data_rd_r <= ctrl_mem [0];
          if (is_addr_chnl1_s)  data_rd_r <= ctrl_mem [1];
          if (is_addr_chnl2_s)  data_rd_r <= ctrl_mem [2];
          //
          if (is_addr_freeslot_0_s)  data_rd_r <= ro_mem [0];
          if (is_addr_freeslot_1_s)  data_rd_r <= ro_mem [1];
          if (is_addr_freeslot_2_s)  data_rd_r <= ro_mem [2];

      end  
  end  
end

assign   prdata_o  = data_rd_r ;
assign   pslverr_o = is_st_acc_s && is_err_rng_s ; 
assign   pready_o  = is_st_acc_s ;

//--------------------------------------------------------------------------------------------------------
//Address decoder
//--------------------------------------------------------------------------------------------------------
assign is_ctrl_rng_s = ~|(addr_r[7:4]) ; //0h0*
assign is_ro_rng_s   = ~|(addr_r[7:5]) && addr_r[4] ; //0h1*
assign is_err_rng_s  =  ~(is_ctrl_rng_s | is_ro_rng_s);

assign idx_0_s       = (addr_r[3:2]==2'b00)? 1'b1 : 1'b0;
assign idx_1_s       = (addr_r[3:2]==2'b01)? 1'b1 : 1'b0;
assign idx_2_s       = (addr_r[3:2]==2'b10)? 1'b1 : 1'b0;

assign is_addr_chnl0_s   = is_ctrl_rng_s & idx_0_s ;
assign is_addr_chnl1_s  = is_ctrl_rng_s & idx_1_s ;
assign is_addr_chnl2_s   = is_ctrl_rng_s & idx_2_s ;

assign is_addr_freeslot_0_s  = is_ro_rng_s && idx_0_s ;
assign is_addr_freeslot_1_s  = is_ro_rng_s && idx_1_s ;
assign is_addr_freeslot_2_s  = is_ro_rng_s && idx_2_s ;

//--------------------------------------------------------------------------------------------------------
// Ctrl Proc
always @ (posedge clk_i or negedge rstn_i) //Trace fifo's margin
begin  : CONTROL_PROC
  if (!rstn_i)
    begin
      ctrl_mem[0] <= 32'h00000007; 
      ctrl_mem[1] <= 32'h00000007; 
      ctrl_mem[2] <= 32'h00000007;
    end else begin
      if (is_st_acc_s & pwr_i) begin
          if (is_addr_chnl0_s) ctrl_mem [0] <= pwdata_i ;
          if (is_addr_chnl1_s) ctrl_mem [1] <= pwdata_i ;
          if (is_addr_chnl2_s) ctrl_mem [2] <= pwdata_i ;
      end 
    end
end

// RO_Proc
always @ (posedge clk_i or negedge rstn_i) //Trace fifo's margin
begin  : RO_PROC
  if (!rstn_i)
    begin
        ro_mem[0] <= 32'h00000020; // slv_free_slot
        ro_mem[1] <= 32'h00000020; // 
        ro_mem[2] <= 32'h00000020; // 
    end else begin
        ro_mem[0][`FIFO_MARGIN_WIDTH-1:0] <= slv0_margin_i; // slv_free_slot
        ro_mem[1][`FIFO_MARGIN_WIDTH-1:0] <= slv1_margin_i; // 
        ro_mem[2][`FIFO_MARGIN_WIDTH-1:0] <= slv2_margin_i; // 
    end
end


assign  slv0_pkglen_o  = ctrl_mem[0][`PAC_LEN_HIGH:`PAC_LEN_LOW];
assign  slv1_pkglen_o  = ctrl_mem[1][`PAC_LEN_HIGH:`PAC_LEN_LOW];
assign  slv2_pkglen_o  = ctrl_mem[2][`PAC_LEN_HIGH:`PAC_LEN_LOW];

assign  slv0_prio_o  = ctrl_mem[0][`PRIO_HIGH:`PRIO_LOW];
assign  slv1_prio_o  = ctrl_mem[1][`PRIO_HIGH:`PRIO_LOW];
assign  slv2_prio_o  = ctrl_mem[2][`PRIO_HIGH:`PRIO_LOW];
  
assign  slv0_en_o = ctrl_mem[0][0];
assign  slv1_en_o = ctrl_mem[1][0];
assign  slv2_en_o = ctrl_mem[2][0];

endmodule
