----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 02.06.2018 00:14:12
-- Design Name: 
-- Module Name: AccelArithmetics - Behavioral
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
--use IEEE.STD_LOGIC_ARITH.ALL;
--use ieee.math_real.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;
use IEEE.std_logic_signed.all;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity AccelArithmetics is
generic 
(
   SYSCLK_FREQUENCY_HZ : integer := 100000000;
   ACC_X_Y_MAX         : STD_LOGIC_VECTOR (9 downto 0) := (others => '1');
   ACC_X_Y_MIN         : STD_LOGIC_VECTOR (9 downto 0) := (others => '0') -- corresponding to -1g
);
port
(
 SYSCLK     : in STD_LOGIC; -- System Clock
 RESET      : in STD_LOGIC;
 
 -- Accelerometer data input signals
 ACCEL_X_IN    : in STD_LOGIC_VECTOR (11 downto 0);
 ACCEL_Y_IN    : in STD_LOGIC_VECTOR (11 downto 0);
 Data_Ready    : in STD_LOGIC;

 -- Accelerometer data output signals to be sent to the VGA display
 ACCEL_X_OUT    : out STD_LOGIC_VECTOR (9 downto 0);
 ACCEL_Y_OUT    : out STD_LOGIC_VECTOR (9 downto 0)
);
end AccelArithmetics;

architecture Behavioral of AccelArithmetics is
-- convert ACCEL_X and ACCEL_Y data to unsigned and divide by 4 
-- (scaled to 0-1023, with -2g=0, 0g=511, 2g=1023)

constant SUM_FACTOR : std_logic_vector (12 downto 0) :=  '0' & X"7FF"; --2047

signal ACCEL_X_SUM : std_logic_vector (12 downto 0) := (others => '0'); 
signal ACCEL_Y_SUM : std_logic_vector (12 downto 0) := (others => '0');

signal ACCEL_X_SUM_SHIFTED : std_logic_vector (9 downto 0) := (others => '0'); -- Divide the sum by four
signal ACCEL_Y_SUM_SHIFTED : std_logic_vector (9 downto 0) := (others => '0');


begin

-- Add 2047 to the incoming acceleration data
-- Therefore ACCEL_X_SUM and ACCEL_Y_SUM will be scaled to 
-- -2g = 0, -1g = 1023, 0g = 2047, 1g = 3071, 2g = 4095
Accel_Sum: process (SYSCLK, RESET, ACCEL_X_IN, ACCEL_Y_IN, Data_Ready)
begin
   if SYSCLK'EVENT and SYSCLK = '1' then
      if RESET = '1' then 
         ACCEL_X_SUM <= (others => '0');
         ACCEL_Y_SUM <= (others => '0');
      elsif Data_Ready = '1' then
         
            ACCEL_X_SUM <= ACCEL_X_IN + SUM_FACTOR;
              
            ACCEL_Y_SUM <=  ACCEL_Y_IN + SUM_FACTOR;
      end if;
   end if;
end process Accel_Sum;

-- Divide by four ACCEL_X_SUM and ACCEL_Y_SUM, therefore will be scaled to
-- -2g = 0, -1g = 255, 0g = 511, 1g = 767, 2g = 1023
ACCEL_X_OUT <= ACCEL_X_SUM(11 downto 2);
-- Invert ACCEL_Y_CLIP data to display on the screen the box movement 
-- on the Y axis according to the board movement
ACCEL_Y_OUT <= (NOT ACCEL_Y_SUM(11 downto 2)) + "0000000001";


end Behavioral;

