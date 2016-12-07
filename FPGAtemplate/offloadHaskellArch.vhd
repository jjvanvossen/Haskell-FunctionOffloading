library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

LIBRARY altera_mf;
USE altera_mf.all;

entity offloadHaskellArch is
  port (
    GPIO_LED 	: OUT std_logic_vector(3 DOWNTO 0);
	 GPIO_KEY  	: IN std_logic_vector(3 DOWNTO 0); -- The 4 FPGA dedicated keys. Key(0) = reset
    clk_bot1 	: IN std_logic;
    OSC_50 		: IN std_logic; -- SoCKit onboard 50 MHz oscillator
	 
    hps_0_hps_io_hps_io_emac1_inst_RXD0 : IN std_logic;
    hps_0_hps_io_hps_io_emac1_inst_RXD1 : IN std_logic;
    hps_0_hps_io_hps_io_emac1_inst_RXD2 : IN std_logic;
    hps_0_hps_io_hps_io_emac1_inst_RXD3 : IN std_logic;
    hps_0_hps_io_hps_io_emac1_inst_RX_CLK : IN std_logic;
    hps_0_hps_io_hps_io_emac1_inst_RX_CTL : IN std_logic;
    hps_0_hps_io_hps_io_spim0_inst_MISO : IN std_logic;
    hps_0_hps_io_hps_io_spim1_inst_MISO : IN std_logic;
    hps_0_hps_io_hps_io_uart0_inst_RX : IN std_logic;
    hps_0_hps_io_hps_io_usb1_inst_CLK : IN std_logic;
    hps_0_hps_io_hps_io_usb1_inst_DIR : IN std_logic;
    hps_0_hps_io_hps_io_usb1_inst_NXT : IN std_logic;
    hps_0_hps_io_hps_io_emac1_inst_MDIO : INOUT std_logic;
    hps_0_hps_io_hps_io_gpio_inst_GPIO00 : INOUT std_logic;
    hps_0_hps_io_hps_io_gpio_inst_GPIO09 : INOUT std_logic;
    hps_0_hps_io_hps_io_gpio_inst_GPIO35 : INOUT std_logic;
    hps_0_hps_io_hps_io_gpio_inst_GPIO48 : INOUT std_logic;
    hps_0_hps_io_hps_io_gpio_inst_GPIO53 : INOUT std_logic;
    hps_0_hps_io_hps_io_gpio_inst_GPIO54 : INOUT std_logic;
    hps_0_hps_io_hps_io_gpio_inst_GPIO55 : INOUT std_logic;
    hps_0_hps_io_hps_io_gpio_inst_GPIO56 : INOUT std_logic;
    hps_0_hps_io_hps_io_gpio_inst_GPIO61 : INOUT std_logic;
    hps_0_hps_io_hps_io_gpio_inst_GPIO62 : INOUT std_logic;
    hps_0_hps_io_hps_io_i2c1_inst_SCL : INOUT std_logic;
    hps_0_hps_io_hps_io_i2c1_inst_SDA : INOUT std_logic;
    hps_0_hps_io_hps_io_qspi_inst_IO0 : INOUT std_logic;
    hps_0_hps_io_hps_io_qspi_inst_IO1 : INOUT std_logic;
    hps_0_hps_io_hps_io_qspi_inst_IO2 : INOUT std_logic;
    hps_0_hps_io_hps_io_qspi_inst_IO3 : INOUT std_logic;
    hps_0_hps_io_hps_io_sdio_inst_CMD : INOUT std_logic;
    hps_0_hps_io_hps_io_sdio_inst_D0 : INOUT std_logic;
    hps_0_hps_io_hps_io_sdio_inst_D1 : INOUT std_logic;
    hps_0_hps_io_hps_io_sdio_inst_D2 : INOUT std_logic;
    hps_0_hps_io_hps_io_sdio_inst_D3 : INOUT std_logic;
    hps_0_hps_io_hps_io_usb1_inst_D0 : INOUT std_logic;
    hps_0_hps_io_hps_io_usb1_inst_D1 : INOUT std_logic;
    hps_0_hps_io_hps_io_usb1_inst_D2 : INOUT std_logic;
    hps_0_hps_io_hps_io_usb1_inst_D3 : INOUT std_logic;
    hps_0_hps_io_hps_io_usb1_inst_D4 : INOUT std_logic;
    hps_0_hps_io_hps_io_usb1_inst_D5 : INOUT std_logic;
    hps_0_hps_io_hps_io_usb1_inst_D6 : INOUT std_logic;
    hps_0_hps_io_hps_io_usb1_inst_D7 : INOUT std_logic;
    hps_0_hps_io_hps_io_emac1_inst_MDC : OUT std_logic;
    hps_0_hps_io_hps_io_emac1_inst_TXD0 : OUT std_logic;
    hps_0_hps_io_hps_io_emac1_inst_TXD1 : OUT std_logic;
    hps_0_hps_io_hps_io_emac1_inst_TXD2 : OUT std_logic;
    hps_0_hps_io_hps_io_emac1_inst_TXD3 : OUT std_logic;
    hps_0_hps_io_hps_io_emac1_inst_TX_CLK : OUT std_logic;
    hps_0_hps_io_hps_io_emac1_inst_TX_CTL : OUT std_logic;
    hps_0_hps_io_hps_io_qspi_inst_CLK : OUT std_logic;
    hps_0_hps_io_hps_io_qspi_inst_SS0 : OUT std_logic;
    hps_0_hps_io_hps_io_sdio_inst_CLK : OUT std_logic;
    hps_0_hps_io_hps_io_spim0_inst_CLK : OUT std_logic;
    hps_0_hps_io_hps_io_spim0_inst_MOSI : OUT std_logic;
    hps_0_hps_io_hps_io_spim0_inst_SS0 : OUT std_logic;
    hps_0_hps_io_hps_io_spim1_inst_CLK : OUT std_logic;
    hps_0_hps_io_hps_io_spim1_inst_MOSI : OUT std_logic;
    hps_0_hps_io_hps_io_spim1_inst_SS0 : OUT std_logic;
    hps_0_hps_io_hps_io_uart0_inst_TX : OUT std_logic;
    hps_0_hps_io_hps_io_usb1_inst_STP : OUT std_logic;
    
    memory_mem_a : OUT std_logic_vector(14 DOWNTO 0);
    memory_mem_ba : OUT std_logic_vector(2 DOWNTO 0);
    memory_mem_cas_n : OUT std_logic;
    memory_mem_ck : OUT std_logic;
    memory_mem_ck_n : OUT std_logic;
    memory_mem_cke : OUT std_logic;
    memory_mem_cs_n : OUT std_logic;
    memory_mem_dm : OUT std_logic_vector(3 DOWNTO 0);
    memory_mem_odt : OUT std_logic;
    memory_mem_ras_n : OUT std_logic;
    memory_mem_reset_n : OUT std_logic;
    memory_mem_we_n : OUT std_logic;
    memory_mem_dq : INOUT std_logic_vector(31 DOWNTO 0);
    memory_mem_dqs : INOUT std_logic_vector(3 DOWNTO 0);
    memory_mem_dqs_n : INOUT std_logic_vector(3 DOWNTO 0);
    memory_oct_rzqin : IN std_logic;
    
    vga_blank_n : OUT std_logic;
    vga_blue : OUT std_logic_vector(7 DOWNTO 0);
    vga_clk_out : OUT std_logic;
    vga_green : OUT std_logic_vector(7 DOWNTO 0);
    vga_hsync : OUT std_logic;
    vga_red : OUT std_logic_vector(7 DOWNTO 0);
    vga_sync_n : OUT std_logic;
    vga_vsync : OUT std_logic);
end offloadHaskellArch;


architecture template_arch of offloadHaskellArch is
  component xillybus
    port (
      clk_bot1 : IN std_logic;
      hps_0_hps_io_hps_io_emac1_inst_RXD0 : IN std_logic;
      hps_0_hps_io_hps_io_emac1_inst_RXD1 : IN std_logic;
      hps_0_hps_io_hps_io_emac1_inst_RXD2 : IN std_logic;
      hps_0_hps_io_hps_io_emac1_inst_RXD3 : IN std_logic;
      hps_0_hps_io_hps_io_emac1_inst_RX_CLK : IN std_logic;
      hps_0_hps_io_hps_io_emac1_inst_RX_CTL : IN std_logic;
      hps_0_hps_io_hps_io_spim0_inst_MISO : IN std_logic;
      hps_0_hps_io_hps_io_spim1_inst_MISO : IN std_logic;
      hps_0_hps_io_hps_io_uart0_inst_RX : IN std_logic;
      hps_0_hps_io_hps_io_usb1_inst_CLK : IN std_logic;
      hps_0_hps_io_hps_io_usb1_inst_DIR : IN std_logic;
      hps_0_hps_io_hps_io_usb1_inst_NXT : IN std_logic;
      memory_oct_rzqin : IN std_logic;
      hps_0_hps_io_hps_io_emac1_inst_MDIO : INOUT std_logic;
      hps_0_hps_io_hps_io_gpio_inst_GPIO00 : INOUT std_logic;
      hps_0_hps_io_hps_io_gpio_inst_GPIO09 : INOUT std_logic;
      hps_0_hps_io_hps_io_gpio_inst_GPIO35 : INOUT std_logic;
      hps_0_hps_io_hps_io_gpio_inst_GPIO48 : INOUT std_logic;
      hps_0_hps_io_hps_io_gpio_inst_GPIO53 : INOUT std_logic;
      hps_0_hps_io_hps_io_gpio_inst_GPIO54 : INOUT std_logic;
      hps_0_hps_io_hps_io_gpio_inst_GPIO55 : INOUT std_logic;
      hps_0_hps_io_hps_io_gpio_inst_GPIO56 : INOUT std_logic;
      hps_0_hps_io_hps_io_gpio_inst_GPIO61 : INOUT std_logic;
      hps_0_hps_io_hps_io_gpio_inst_GPIO62 : INOUT std_logic;
      hps_0_hps_io_hps_io_i2c1_inst_SCL : INOUT std_logic;
      hps_0_hps_io_hps_io_i2c1_inst_SDA : INOUT std_logic;
      hps_0_hps_io_hps_io_qspi_inst_IO0 : INOUT std_logic;
      hps_0_hps_io_hps_io_qspi_inst_IO1 : INOUT std_logic;
      hps_0_hps_io_hps_io_qspi_inst_IO2 : INOUT std_logic;
      hps_0_hps_io_hps_io_qspi_inst_IO3 : INOUT std_logic;
      hps_0_hps_io_hps_io_sdio_inst_CMD : INOUT std_logic;
      hps_0_hps_io_hps_io_sdio_inst_D0 : INOUT std_logic;
      hps_0_hps_io_hps_io_sdio_inst_D1 : INOUT std_logic;
      hps_0_hps_io_hps_io_sdio_inst_D2 : INOUT std_logic;
      hps_0_hps_io_hps_io_sdio_inst_D3 : INOUT std_logic;
      hps_0_hps_io_hps_io_usb1_inst_D0 : INOUT std_logic;
      hps_0_hps_io_hps_io_usb1_inst_D1 : INOUT std_logic;
      hps_0_hps_io_hps_io_usb1_inst_D2 : INOUT std_logic;
      hps_0_hps_io_hps_io_usb1_inst_D3 : INOUT std_logic;
      hps_0_hps_io_hps_io_usb1_inst_D4 : INOUT std_logic;
      hps_0_hps_io_hps_io_usb1_inst_D5 : INOUT std_logic;
      hps_0_hps_io_hps_io_usb1_inst_D6 : INOUT std_logic;
      hps_0_hps_io_hps_io_usb1_inst_D7 : INOUT std_logic;
      memory_mem_dq : INOUT std_logic_vector(31 DOWNTO 0);
      memory_mem_dqs : INOUT std_logic_vector(3 DOWNTO 0);
      memory_mem_dqs_n : INOUT std_logic_vector(3 DOWNTO 0);
      --GPIO_LED : OUT std_logic_vector(3 DOWNTO 0);
      bus_clk : OUT std_logic;
      hps_0_hps_io_hps_io_emac1_inst_MDC : OUT std_logic;
      hps_0_hps_io_hps_io_emac1_inst_TXD0 : OUT std_logic;
      hps_0_hps_io_hps_io_emac1_inst_TXD1 : OUT std_logic;
      hps_0_hps_io_hps_io_emac1_inst_TXD2 : OUT std_logic;
      hps_0_hps_io_hps_io_emac1_inst_TXD3 : OUT std_logic;
      hps_0_hps_io_hps_io_emac1_inst_TX_CLK : OUT std_logic;
      hps_0_hps_io_hps_io_emac1_inst_TX_CTL : OUT std_logic;
      hps_0_hps_io_hps_io_qspi_inst_CLK : OUT std_logic;
      hps_0_hps_io_hps_io_qspi_inst_SS0 : OUT std_logic;
      hps_0_hps_io_hps_io_sdio_inst_CLK : OUT std_logic;
      hps_0_hps_io_hps_io_spim0_inst_CLK : OUT std_logic;
      hps_0_hps_io_hps_io_spim0_inst_MOSI : OUT std_logic;
      hps_0_hps_io_hps_io_spim0_inst_SS0 : OUT std_logic;
      hps_0_hps_io_hps_io_spim1_inst_CLK : OUT std_logic;
      hps_0_hps_io_hps_io_spim1_inst_MOSI : OUT std_logic;
      hps_0_hps_io_hps_io_spim1_inst_SS0 : OUT std_logic;
      hps_0_hps_io_hps_io_uart0_inst_TX : OUT std_logic;
      hps_0_hps_io_hps_io_usb1_inst_STP : OUT std_logic;
      memory_mem_a : OUT std_logic_vector(14 DOWNTO 0);
      memory_mem_ba : OUT std_logic_vector(2 DOWNTO 0);
      memory_mem_cas_n : OUT std_logic;
      memory_mem_ck : OUT std_logic;
      memory_mem_ck_n : OUT std_logic;
      memory_mem_cke : OUT std_logic;
      memory_mem_cs_n : OUT std_logic;
      memory_mem_dm : OUT std_logic_vector(3 DOWNTO 0);
      memory_mem_odt : OUT std_logic;
      memory_mem_ras_n : OUT std_logic;
      memory_mem_reset_n : OUT std_logic;
      memory_mem_we_n : OUT std_logic;
      quiesce : OUT std_logic;
      vga_blank_n : OUT std_logic;
      vga_blue : OUT std_logic_vector(7 DOWNTO 0);
      vga_clk_out : OUT std_logic;
      vga_green : OUT std_logic_vector(7 DOWNTO 0);
      vga_hsync : OUT std_logic;
      vga_red : OUT std_logic_vector(7 DOWNTO 0);
      vga_sync_n : OUT std_logic;
      vga_vsync : OUT std_logic;
      user_r_read_32_rden : OUT std_logic;
      user_r_read_32_empty : IN std_logic;
      user_r_read_32_data : IN std_logic_vector(31 DOWNTO 0);
      user_r_read_32_eof : IN std_logic;
      user_r_read_32_open : OUT std_logic;
      user_w_write_32_wren : OUT std_logic;
      user_w_write_32_full : IN std_logic;
      user_w_write_32_data : OUT std_logic_vector(31 DOWNTO 0);
      user_w_write_32_open : OUT std_logic);
  end component;
  
  -- single clock FIFO component  
  COMPONENT scfifo
    GENERIC (
      add_ram_output_register	: STRING;
      intended_device_family	: STRING;
      lpm_numwords		: NATURAL;
      lpm_showahead		: STRING;
      lpm_type		        : STRING;
      lpm_width		        : NATURAL;
      lpm_widthu		: NATURAL;
      overflow_checking		: STRING;
      underflow_checking	: STRING;
      use_eab		        : STRING
      );
    PORT (
      clock	: IN STD_LOGIC ;
      data	: IN STD_LOGIC_VECTOR ( (lpm_width - 1) DOWNTO 0);
      rdreq	: IN STD_LOGIC ;
      sclr	: IN STD_LOGIC ;
      empty	: OUT STD_LOGIC ;
      full	: OUT STD_LOGIC ;
      q	: OUT STD_LOGIC_VECTOR ( (lpm_width - 1) DOWNTO 0);
      wrreq	: IN STD_LOGIC 
      );
  END COMPONENT;
  
  -- dual clock FIFO component
  COMPONENT dcfifo
    GENERIC (
      add_ram_output_register	: STRING;
      intended_device_family	: STRING;
      lpm_numwords		: NATURAL;
      lpm_showahead		: STRING;
      lpm_type		        : STRING;
      lpm_width		        : NATURAL;
      lpm_widthu		: NATURAL;
      overflow_checking		: STRING;
      underflow_checking	: STRING;
      use_eab		        : STRING
      );
    PORT (
      wrclk	: IN STD_LOGIC ;
		rdclk : IN STD_LOGIC ;
		wrreq : IN STD_LOGIC ;
      rdreq : IN STD_LOGIC ;
      data	: IN STD_LOGIC_VECTOR ( (lpm_width - 1) DOWNTO 0);
      aclr	: IN STD_LOGIC ;
      rdempty	: OUT STD_LOGIC ;
      wrfull	: OUT STD_LOGIC ;
      q	: OUT STD_LOGIC_VECTOR ( (lpm_width - 1) DOWNTO 0)
      );
  END COMPONENT;
	
	-- =====================================================================
	-- User components
	-- =====================================================================
	
	-- Offloaded functions FPGA Architecture top-entity component
	component SoCKit_Offloaded_Haskell_functions is
		port(data_in         : in signed(31 downto 0);
       empty_in        : in boolean;
       full_out        : in boolean;       
       system1000      : in std_logic;-- clock      
       system1000_rstn : in std_logic;-- asynchronous reset: active low
       data_o          : out signed(31 downto 0);
       wren_out        : out boolean;
       rden_in         : out boolean;
       err_r_mux       : out boolean;
       err_r_s2p       : out boolean;
       err_w_p2s       : out boolean;
       err_w_demux     : out boolean);
		end component;

  signal bus_clk :  std_logic;
  
  signal quiesce : std_logic;
  signal user_r_read_32_rden :  std_logic;
  signal user_r_read_32_empty :  std_logic;
  signal user_r_read_32_data :  std_logic_vector(31 DOWNTO 0);
  signal user_r_read_32_eof :  std_logic;
  signal user_r_read_32_open :  std_logic;
  signal user_w_write_32_wren :  std_logic;
  signal user_w_write_32_full :  std_logic;
  signal user_w_write_32_data :  std_logic_vector(31 DOWNTO 0);
  signal user_w_write_32_open :  std_logic;
  
  	-- =====================================================================
	-- User signals
	-- =====================================================================
  
  -- wrapper fifo interfacing signals
  signal wrapper_r_read_32_data : std_logic_vector(31 DOWNTO 0);
  signal wrapper_r_read_32_empty : std_logic;
  signal wrapper_r_read_32_rden : std_logic; 
  signal wrapper_w_write_32_data : std_logic_vector(31 DOWNTO 0);
  signal wrapper_w_write_32_full : std_logic;
  signal wrapper_w_write_32_wren : std_logic;
  
   -- conversions needed to coerce between bool and 1-bit std logic
  	function Bool2StdLogic(B: boolean) return std_logic is
		begin
			if B then
				return('1');
			else
				return('0');
			end if;
		end function Bool2StdLogic;	
		
	function StdLogic2Bool(L: std_logic) return boolean is
		begin
			if (L = '1') then
				return(True);
			else
				return(False);
			end if;
		end function StdLogic2Bool;

begin
  xillybus_ins : xillybus
    port map (      
      -- Ports related to /dev/xillybus_read_32
      -- FPGA to CPU signals:
      user_r_read_32_rden => user_r_read_32_rden,
      user_r_read_32_empty => user_r_read_32_empty,
      user_r_read_32_data => user_r_read_32_data,
      user_r_read_32_eof => user_r_read_32_eof,
      user_r_read_32_open => user_r_read_32_open,

      -- Ports related to /dev/xillybus_write_32
      -- CPU to FPGA signals:
      user_w_write_32_wren => user_w_write_32_wren,
      user_w_write_32_full => user_w_write_32_full,
      user_w_write_32_data => user_w_write_32_data,
      user_w_write_32_open => user_w_write_32_open,

      -- General signals
      clk_bot1 => clk_bot1,
      hps_0_hps_io_hps_io_emac1_inst_RXD0 => hps_0_hps_io_hps_io_emac1_inst_RXD0,
      hps_0_hps_io_hps_io_emac1_inst_RXD1 => hps_0_hps_io_hps_io_emac1_inst_RXD1,
      hps_0_hps_io_hps_io_emac1_inst_RXD2 => hps_0_hps_io_hps_io_emac1_inst_RXD2,
      hps_0_hps_io_hps_io_emac1_inst_RXD3 => hps_0_hps_io_hps_io_emac1_inst_RXD3,
      hps_0_hps_io_hps_io_emac1_inst_RX_CLK => hps_0_hps_io_hps_io_emac1_inst_RX_CLK,
      hps_0_hps_io_hps_io_emac1_inst_RX_CTL => hps_0_hps_io_hps_io_emac1_inst_RX_CTL,
      hps_0_hps_io_hps_io_spim0_inst_MISO => hps_0_hps_io_hps_io_spim0_inst_MISO,
      hps_0_hps_io_hps_io_spim1_inst_MISO => hps_0_hps_io_hps_io_spim1_inst_MISO,
      hps_0_hps_io_hps_io_uart0_inst_RX => hps_0_hps_io_hps_io_uart0_inst_RX,
      hps_0_hps_io_hps_io_usb1_inst_CLK => hps_0_hps_io_hps_io_usb1_inst_CLK,
      hps_0_hps_io_hps_io_usb1_inst_DIR => hps_0_hps_io_hps_io_usb1_inst_DIR,
      hps_0_hps_io_hps_io_usb1_inst_NXT => hps_0_hps_io_hps_io_usb1_inst_NXT,
      memory_oct_rzqin => memory_oct_rzqin,
      hps_0_hps_io_hps_io_emac1_inst_MDIO => hps_0_hps_io_hps_io_emac1_inst_MDIO,
      hps_0_hps_io_hps_io_gpio_inst_GPIO00 => hps_0_hps_io_hps_io_gpio_inst_GPIO00,
      hps_0_hps_io_hps_io_gpio_inst_GPIO09 => hps_0_hps_io_hps_io_gpio_inst_GPIO09,
      hps_0_hps_io_hps_io_gpio_inst_GPIO35 => hps_0_hps_io_hps_io_gpio_inst_GPIO35,
      hps_0_hps_io_hps_io_gpio_inst_GPIO48 => hps_0_hps_io_hps_io_gpio_inst_GPIO48,
      hps_0_hps_io_hps_io_gpio_inst_GPIO53 => hps_0_hps_io_hps_io_gpio_inst_GPIO53,
      hps_0_hps_io_hps_io_gpio_inst_GPIO54 => hps_0_hps_io_hps_io_gpio_inst_GPIO54,
      hps_0_hps_io_hps_io_gpio_inst_GPIO55 => hps_0_hps_io_hps_io_gpio_inst_GPIO55,
      hps_0_hps_io_hps_io_gpio_inst_GPIO56 => hps_0_hps_io_hps_io_gpio_inst_GPIO56,
      hps_0_hps_io_hps_io_gpio_inst_GPIO61 => hps_0_hps_io_hps_io_gpio_inst_GPIO61,
      hps_0_hps_io_hps_io_gpio_inst_GPIO62 => hps_0_hps_io_hps_io_gpio_inst_GPIO62,
      hps_0_hps_io_hps_io_i2c1_inst_SCL => hps_0_hps_io_hps_io_i2c1_inst_SCL,
      hps_0_hps_io_hps_io_i2c1_inst_SDA => hps_0_hps_io_hps_io_i2c1_inst_SDA,
      hps_0_hps_io_hps_io_qspi_inst_IO0 => hps_0_hps_io_hps_io_qspi_inst_IO0,
      hps_0_hps_io_hps_io_qspi_inst_IO1 => hps_0_hps_io_hps_io_qspi_inst_IO1,
      hps_0_hps_io_hps_io_qspi_inst_IO2 => hps_0_hps_io_hps_io_qspi_inst_IO2,
      hps_0_hps_io_hps_io_qspi_inst_IO3 => hps_0_hps_io_hps_io_qspi_inst_IO3,
      hps_0_hps_io_hps_io_sdio_inst_CMD => hps_0_hps_io_hps_io_sdio_inst_CMD,
      hps_0_hps_io_hps_io_sdio_inst_D0 => hps_0_hps_io_hps_io_sdio_inst_D0,
      hps_0_hps_io_hps_io_sdio_inst_D1 => hps_0_hps_io_hps_io_sdio_inst_D1,
      hps_0_hps_io_hps_io_sdio_inst_D2 => hps_0_hps_io_hps_io_sdio_inst_D2,
      hps_0_hps_io_hps_io_sdio_inst_D3 => hps_0_hps_io_hps_io_sdio_inst_D3,
      hps_0_hps_io_hps_io_usb1_inst_D0 => hps_0_hps_io_hps_io_usb1_inst_D0,
      hps_0_hps_io_hps_io_usb1_inst_D1 => hps_0_hps_io_hps_io_usb1_inst_D1,
      hps_0_hps_io_hps_io_usb1_inst_D2 => hps_0_hps_io_hps_io_usb1_inst_D2,
      hps_0_hps_io_hps_io_usb1_inst_D3 => hps_0_hps_io_hps_io_usb1_inst_D3,
      hps_0_hps_io_hps_io_usb1_inst_D4 => hps_0_hps_io_hps_io_usb1_inst_D4,
      hps_0_hps_io_hps_io_usb1_inst_D5 => hps_0_hps_io_hps_io_usb1_inst_D5,
      hps_0_hps_io_hps_io_usb1_inst_D6 => hps_0_hps_io_hps_io_usb1_inst_D6,
      hps_0_hps_io_hps_io_usb1_inst_D7 => hps_0_hps_io_hps_io_usb1_inst_D7,
      memory_mem_dq => memory_mem_dq,
      memory_mem_dqs => memory_mem_dqs,
      memory_mem_dqs_n => memory_mem_dqs_n,
      --GPIO_LED => GPIO_LED,
      bus_clk => bus_clk,
      hps_0_hps_io_hps_io_emac1_inst_MDC => hps_0_hps_io_hps_io_emac1_inst_MDC,
      hps_0_hps_io_hps_io_emac1_inst_TXD0 => hps_0_hps_io_hps_io_emac1_inst_TXD0,
      hps_0_hps_io_hps_io_emac1_inst_TXD1 => hps_0_hps_io_hps_io_emac1_inst_TXD1,
      hps_0_hps_io_hps_io_emac1_inst_TXD2 => hps_0_hps_io_hps_io_emac1_inst_TXD2,
      hps_0_hps_io_hps_io_emac1_inst_TXD3 => hps_0_hps_io_hps_io_emac1_inst_TXD3,
      hps_0_hps_io_hps_io_emac1_inst_TX_CLK => hps_0_hps_io_hps_io_emac1_inst_TX_CLK,
      hps_0_hps_io_hps_io_emac1_inst_TX_CTL => hps_0_hps_io_hps_io_emac1_inst_TX_CTL,
      hps_0_hps_io_hps_io_qspi_inst_CLK => hps_0_hps_io_hps_io_qspi_inst_CLK,
      hps_0_hps_io_hps_io_qspi_inst_SS0 => hps_0_hps_io_hps_io_qspi_inst_SS0,
      hps_0_hps_io_hps_io_sdio_inst_CLK => hps_0_hps_io_hps_io_sdio_inst_CLK,
      hps_0_hps_io_hps_io_spim0_inst_CLK => hps_0_hps_io_hps_io_spim0_inst_CLK,
      hps_0_hps_io_hps_io_spim0_inst_MOSI => hps_0_hps_io_hps_io_spim0_inst_MOSI,
      hps_0_hps_io_hps_io_spim0_inst_SS0 => hps_0_hps_io_hps_io_spim0_inst_SS0,
      hps_0_hps_io_hps_io_spim1_inst_CLK => hps_0_hps_io_hps_io_spim1_inst_CLK,
      hps_0_hps_io_hps_io_spim1_inst_MOSI => hps_0_hps_io_hps_io_spim1_inst_MOSI,
      hps_0_hps_io_hps_io_spim1_inst_SS0 => hps_0_hps_io_hps_io_spim1_inst_SS0,
      hps_0_hps_io_hps_io_uart0_inst_TX => hps_0_hps_io_hps_io_uart0_inst_TX,
      hps_0_hps_io_hps_io_usb1_inst_STP => hps_0_hps_io_hps_io_usb1_inst_STP,
      memory_mem_a => memory_mem_a,
      memory_mem_ba => memory_mem_ba,
      memory_mem_cas_n => memory_mem_cas_n,
      memory_mem_ck => memory_mem_ck,
      memory_mem_ck_n => memory_mem_ck_n,
      memory_mem_cke => memory_mem_cke,
      memory_mem_cs_n => memory_mem_cs_n,
      memory_mem_dm => memory_mem_dm,
      memory_mem_odt => memory_mem_odt,
      memory_mem_ras_n => memory_mem_ras_n,
      memory_mem_reset_n => memory_mem_reset_n,
      memory_mem_we_n => memory_mem_we_n,
      quiesce => quiesce,
      vga_blank_n => vga_blank_n,
      vga_blue => vga_blue,
      vga_clk_out => vga_clk_out,
      vga_green => vga_green,
      vga_hsync => vga_hsync,
      vga_red => vga_red,
      vga_sync_n => vga_sync_n,
      vga_vsync => vga_vsync
  );
  	
   -- =====================================================================
	-- xillybus_read & xillybus_write FIFO buffer mappings
	-- =====================================================================

	 fifo_32_read : dcfifo
    GENERIC MAP (
      add_ram_output_register => "OFF",
      intended_device_family => "Stratix IV",
      lpm_numwords => 512, -- FIFO depth
      lpm_showahead => "OFF",
      lpm_type => "dcfifo",
      lpm_width => 32,
      lpm_widthu => 9,
      overflow_checking => "ON",
      underflow_checking => "ON",
      use_eab => "ON"
      )
    PORT MAP (
		wrclk => bus_clk,
		rdclk => OSC_50, -- Should be same as FPGA Arch clock
		wrreq => user_w_write_32_wren,
		rdreq => wrapper_r_read_32_rden,
		data => user_w_write_32_data,
		aclr => (NOT GPIO_KEY(0)), --OR NOT (user_w_write_32_open OR user_r_read_32_open), -- Active high. can be used to reset automatically in pipelined mode
		rdempty => wrapper_r_read_32_empty,
      wrfull => user_w_write_32_full,
      q => wrapper_r_read_32_data
    );  
  
	 fifo_32_write : dcfifo
    GENERIC MAP (
      add_ram_output_register => "OFF",
      intended_device_family => "Stratix IV",
      lpm_numwords => 512, -- FIFO depth
      lpm_showahead => "OFF",
      lpm_type => "dcfifo",
      lpm_width => 32,
      lpm_widthu => 9,
      overflow_checking => "ON",
      underflow_checking => "ON",
      use_eab => "ON"
      )
    PORT MAP (
		wrclk => OSC_50, -- Should be same as FPGA Arch clock
		rdclk => bus_clk,
		wrreq => wrapper_w_write_32_wren,
		rdreq => user_r_read_32_rden,
		data => wrapper_w_write_32_data,
		aclr => (NOT GPIO_KEY(0)), --OR NOT (user_w_write_32_open OR user_r_read_32_open), -- Active high. can be used to reset automatically in pipelined mode
		rdempty => user_r_read_32_empty,
      wrfull => wrapper_w_write_32_full,
      q => user_r_read_32_data
    );  

    user_r_read_32_eof <= '0';    
	 
   -- =====================================================================
	-- Port mapping for the top-entity of the offloading FPGA Architecture  
	-- =====================================================================	 
		
	HaskellFns : SoCKit_Offloaded_Haskell_functions
		port map(
				data_in         				=> signed(wrapper_r_read_32_data),
				empty_in        				=> stdLogic2Bool(wrapper_r_read_32_empty),
				full_out        				=> stdLogic2Bool(wrapper_w_write_32_full),
				system1000      				=> OSC_50, -- clock (can be scaled down here with a PLL for example)
				system1000_rstn 				=> GPIO_KEY(0), --AND (user_w_write_32_open OR user_r_read_32_open), -- active low. -- can be used to reset automatically in pipelined mode
				std_logic_vector(data_o) 	=> wrapper_w_write_32_data,
				Bool2StdLogic(wren_out)  	=> wrapper_w_write_32_wren,
				Bool2StdLogic(rden_in)   	=> wrapper_r_read_32_rden,
				Bool2StdLogic(err_r_mux)   => GPIO_LED(0),
				Bool2StdLogic(err_r_s2p)   => GPIO_LED(1),
				Bool2StdLogic(err_w_p2s)   => GPIO_LED(2),
				Bool2StdLogic(err_w_demux) => GPIO_LED(3)
		);
	
	 
end template_arch;
