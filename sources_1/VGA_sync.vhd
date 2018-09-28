----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 23.05.2018 17:37:58
-- Design Name: 
-- Module Name: VGA_Sync - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity VGA_Sync is
    Port ( clk : in STD_LOGIC;
           hsync : out STD_LOGIC;
           vsync : out STD_LOGIC;
           video_on : out STD_LOGIC; 
           clr : in STD_LOGIC;
           x : out integer;
           y : out integer
           
    );
end VGA_Sync;

architecture Behavioral of VGA_Sync is
    constant hpixels : integer := 640;
    constant vpixels : integer := 480;
    
    constant vfp : integer := 10;
    constant hfp : integer := 16;
    
    constant vsp : integer := 2;
    constant hsp : integer := 96;
    
    constant vbp : integer := 33;
    constant hbp : integer := 48;
    
    
    signal hc : integer := 0;
    signal vc : integer := 0;
    signal hend : STD_LOGIC := '0';
    signal clk25 : std_logic;
    
    component clk_div
    port
     (-- Clock in ports
      -- Clock out ports
      clk_out1          : out    std_logic;
      clk_in1           : in     std_logic
     );
    end component;
    
begin

clk_25M : clk_div
   port map ( 
  -- Clock out ports  
   clk_out1 => clk25,
   -- Clock in ports
   clk_in1 => clk
 );

process(clk25,clr)
begin
    if(clr = '1') then
        hc <= 0;
    elsif(rising_edge(clk25)) then
        if(hc < hfp + hbp + hsp + hpixels - 1) then
            hc <= hc +1;
            hend <= '0';
        else 
            hc <= 0;
            hend <= '1';        
        end if;
    end if;    
end process;

process(clk25,clr)
begin
    if(clr = '1') then
        vc <= 0;
    elsif(rising_edge(clk25) and hend = '1') then
        if(vc < vfp + vbp + vsp + vpixels - 1) then
                vc <= vc +1;
            else 
                vc <= 0;
            end if;
    end if;
end process;

hsync <= '1' when (hc >= (hpixels + hfp) and (hc < (hpixels + hfp + hsp))) else '0';
vsync <= '1' when (vc >= (vpixels + vfp) and (vc < (vpixels + vfp + vsp))) else '0';

video_on <= '1' when hc < hpixels and vc < vpixels else '0';

x <= hc;
y <= vc;

end Behavioral;
