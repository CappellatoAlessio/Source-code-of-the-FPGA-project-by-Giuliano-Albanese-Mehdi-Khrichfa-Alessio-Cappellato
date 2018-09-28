----------------------------------------------------------------------------------
-- Company: Politecnico di Torino
-- Engineer: Giuliano Albanese, Alessio Cappellato, Mehdi Khrichfa
-- 
-- Create Date: 06.06.2018 15:30:30
-- Design Name: A - Maze - Ing
-- Module Name: top_level - Behavioral
-- Project Name: Computer Architecture 2018
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
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity top_level is
  Port (clk : in STD_LOGIC;
        sw : in STD_LOGIC_VECTOR(1 downto 0);
        rst : in STD_LOGIC;
        sclk : out STD_LOGIC;
        mosi : out STD_LOGIC;
        miso : in STD_LOGIC;
        ss : out STD_LOGIC;
        hsync : out STD_LOGIC;
        vsync : out STD_LOGIC;
        rgb : out STD_LOGIC_VECTOR(0 to 11));
end top_level;

architecture Behavioral of top_level is

COMPONENT blk_mem_gen_0
  PORT (
    clka : IN STD_LOGIC;
    addra : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(2 DOWNTO 0);
    clkb : IN STD_LOGIC;
    addrb : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
    doutb : OUT STD_LOGIC_VECTOR(2 DOWNTO 0)
  );
END COMPONENT;

COMPONENT blk_mem_gen_1
  PORT (
    clka : IN STD_LOGIC;
    addra : IN STD_LOGIC_VECTOR(18 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(0 DOWNTO 0)
  );
END COMPONENT;

COMPONENT VGA_Sync
    Port (
    clk : in STD_LOGIC;
    hsync : out STD_LOGIC;
    vsync : out STD_LOGIC;
    video_on : out STD_LOGIC; 
    clr : in STD_LOGIC;
    x : out integer;
    y : out integer
  );
 END COMPONENT;
 
 component AccelerometerCtl is
     generic 
     (
        SYSCLK_FREQUENCY_HZ : integer := 100000000;
        SCLK_FREQUENCY_HZ   : integer := 1000000;
        NUM_READS_AVG       : integer := 16;
        UPDATE_FREQUENCY_HZ : integer := 1000
     );
     port
     (
      SYSCLK     : in STD_LOGIC; -- System Clock
      RESET      : in STD_LOGIC; -- Reset button on the Nexys4 board is active low
     
      -- SPI interface Signals
      SCLK       : out STD_LOGIC;
      MOSI       : out STD_LOGIC;
      MISO       : in STD_LOGIC;
      SS         : out STD_LOGIC;
     
     -- Accelerometer data signals
      ACCEL_X_OUT    : out STD_LOGIC_VECTOR (9 downto 0);
      ACCEL_Y_OUT    : out STD_LOGIC_VECTOR (9 downto 0)
     );
     end component;
    
    -- Accelerometer buffer registers and control/status signals
    signal ACCEL_X    : STD_LOGIC_VECTOR (9 downto 0);
    signal ACCEL_Y    : STD_LOGIC_VECTOR (9 downto 0);
    signal not_rst    : STD_LOGIC;
    
    -- FSMs state variable definition
	type StateType is (ZERO, A, B, C, D, DB, DC, E, F);
	
	--Rom0 interface parameters
	signal addra, addrb : STD_LOGIC_VECTOR(11 DOWNTO 0) := "000000000000";
	signal douta, doutb : STD_LOGIC_VECTOR(2 DOWNTO 0);
	
	--Rom1 interface paramters
	signal addrl : STD_LOGIC_VECTOR(18 DOWNTO 0);
	signal doutl : STD_LOGIC_VECTOR(0 downto 0);
	
	--VGA sync parameters
	signal hsync_signal, vsync_signal, video_on : STD_LOGIC;
	signal x, y : integer;

	-- Load-maze or logo FSM parameters
	signal buffer_3 : integer := 0;
	signal status : StateType := A;
	constant show_logo : integer:= 240;
	signal logo_counter : integer:= 0;
	signal inner_logo_counter : integer:= 0;
	signal showing_logo : std_logic:= '0';
	signal reset : std_logic;
	
	
	-- Check-collisions FSM parameters
    signal State : StateType;
	signal busy, done : STD_LOGIC;
	signal buffer_1, buffer_2, buffer_4 : integer;
	signal xspeed, yspeed, xspeedacc, yspeedacc, update_freqx, update_freqy : integer;
	signal update_accx, update_accy : integer := 0;
	signal check_left_or_right, check_up_or_down, check_diagonal : integer;
	
	-- Square position parameters
	signal start : integer := 26;
	signal xpos, ypos : integer := 26;
	constant xsize, ysize : integer := 2;

	--Game interface parameters
	signal level : integer;
	constant lev0 : integer := 0;
	constant lev1 : integer := 1200;
	constant lev2 : integer := 2400;
	
	--Ball parameters
	constant BALL_RADIUS: integer := 4;
	
	type rom_type is array (0 to 8) of std_logic_vector (0 to 8);
	--Ball bit-map rom definition
	constant ball_rom : rom_type :=
	(
	   "000111000",
	   "001111100",
	   "111111110",
	   "111111111",
	   "111111111",
	   "011111111",
	   "011111110",
	   "001111100",
	   "000111000"
	);
	signal rom_data: std_logic_vector(0 to 8);
	signal rom_addr : integer range 0 to 8;
	signal rom_bit : integer range 0 to 8;
	type CHECK is array (0 to 8) of integer range 0 to 8;
	constant rightball_checkx : CHECK := (5, 6, 7, 8, 8, 8, 7, 6, 5);
    constant ball_checky : CHECK := (0, 1, 2, 3, 4, 5, 6, 7, 8);
    constant leftball_checkx : CHECK := (3, 2, 1, 0, 0, 0, 1, 2, 3);
    constant ball_checkx : CHECK := (0, 1, 2, 3, 4, 5, 6, 7, 8);
    constant upball_checky : CHECK := (3, 2, 1, 0, 0, 0, 1, 2, 3);
    constant downball_checky : CHECK := (5, 6, 7, 8, 8, 8, 7, 6, 5);
	

begin
    TPROM : blk_mem_gen_0
      PORT MAP (
        clka => clk,
        addra => addra,
        douta => douta,
        clkb => clk,
        addrb => addrb,
        doutb => doutb
      );
      
      
    LOGO : blk_mem_gen_1
        PORT MAP (
          clka => clk,
          addra => addrl,
          douta => doutl
        );
      
    VGA : VGA_Sync
      PORT MAP (
          clk => clk,
          hsync => hsync_signal,
          vsync => vsync_signal,
          video_on => video_on,
          clr => '0',
          x => x,
          y => y
        );
        
    Inst_AccelerometerCtl: AccelerometerCtl
          generic map
          (
               SYSCLK_FREQUENCY_HZ   => 100000000,
               SCLK_FREQUENCY_HZ     => 100000,
               NUM_READS_AVG         => 16,
               UPDATE_FREQUENCY_HZ   => 1000
          )
          port map
          (
              SYSCLK     => clk,
              RESET      => not_rst, 
              -- Spi interface Signals
              SCLK       => sclk,
              MOSI       => mosi,
              MISO       => miso,
              SS         => ss,
              
             -- Accelerometer data signals
              ACCEL_X_OUT   => ACCEL_X,
              ACCEL_Y_OUT   => ACCEL_Y
          );
          
    not_rst <= NOT rst;
        
    hsync <= hsync_signal;
    vsync <= vsync_signal;
    
    rom_addr <= (y - ypos) + BALL_RADIUS when y > ypos else BALL_RADIUS - (ypos - y);
    rom_bit <= (x - xpos) + BALL_RADIUS when x > xpos else BALL_RADIUS - (xpos - x); 
    
    -- background
    process(clk, x, y)
    begin
        if(rising_edge(clk)) then
            CASE Status is
                when A =>
                        if(video_on = '1') then
                            addra <= std_logic_vector(to_unsigned((x/16)+40*(y/16)+level, addra'length));
                            if(showing_logo = '1') then
                                addrl <= std_logic_vector(to_unsigned(x + 640*y, addrl'length));
                            end if;
                        end if;
                        Status <= B;
                when B =>
                    if(buffer_3 = 1) then
                        Status <= C;
                        buffer_3 <= 0;
                        if(video_on = '1') then
                           if(showing_logo = '1') then
                                if(doutl = "0") then
                                    rgb <= "111111111111"; -- white
                                elsif(doutl = "1") then
                                    rgb <= "000000000000"; -- black
                                end if;
                           else 
                                if(douta = "000") then
                                    rgb <= "000000000000"; -- black      / wall
                                elsif(douta = "001") then 
                                    rgb <= "111111111111"; -- white      / corridor
                                elsif(douta = "101") then
                                    rgb <= "000000001111"; -- blue       / teleport (odd symmetry)
                                elsif(douta = "100") then
                                    rgb <= "111100001111"; -- red&blue   / speed-up
                                elsif(douta = "011") then
                                    rgb <= "111111110000"; -- red&green  / hole
                                else
                                    rgb <= "111100000000"; -- red        / arrival
                                end if;
                                if((x >= xpos - BALL_RADIUS and x<=xpos+BALL_RADIUS) and (y >= ypos - BALL_RADIUS and y<=ypos+BALL_RADIUS)) then
                                    rom_data <= ball_rom(rom_addr);
                                    if(rom_data(rom_bit) = '1') then 
                                        rgb <= "000011110000"; -- green / ball
                                    end if;
                                end if;
                            end if;
                        else 
                            rgb <= "000000000000"; -- black
                        end if;
                    else
                        buffer_3 <= buffer_3 + 1;
                    end if;
                when others => status <= A;
                end CASE;
            end if;
     end process;
     
     check_left_or_right <= (xpos - BALL_RADIUS + xspeedacc - 1) / 16 + 40*(ypos/16) + level when xspeedacc < 0
                            else (xpos + BALL_RADIUS + xspeedacc) / 16 + 40*(ypos/16) + level;
     
     check_up_or_down  <= (xpos / 16) + 40*((ypos - BALL_RADIUS + yspeedacc)/16) + level when yspeedacc < 0
                          else (xpos / 16) + 40*((ypos + BALL_RADIUS + yspeedacc)/16) + level;
                          
    check_diagonal <=  (xpos - BALL_RADIUS + xspeedacc - 1) / 16 + 40*((ypos - BALL_RADIUS + yspeedacc)/16) + level when xspeedacc < 0 and yspeedacc < 0 else
                       (xpos + BALL_RADIUS + xspeedacc) / 16  + 40*((ypos - BALL_RADIUS + yspeedacc)/16) + level when xspeedacc >= 0 and yspeedacc < 0 else
                       (xpos + BALL_RADIUS + xspeedacc) / 16  + 40*((ypos + BALL_RADIUS + yspeedacc)/16) + level when xspeedacc >= 0 and yspeedacc >= 0 else
                       (xpos - BALL_RADIUS + xspeedacc - 1) / 16 + 40*((ypos + BALL_RADIUS + yspeedacc)/16) + level;                       

    xspeedacc <=  -4 when ACCEL_X >= 0   and ACCEL_X < 119 else
				  -3 when ACCEL_X >= 119 and ACCEL_X < 239 else
                  -2 when ACCEL_X >= 239 and ACCEL_X < 359 else
                  -1 when ACCEL_X >= 359 and ACCEL_X < 479 else
                   0 when ACCEL_X >= 479 and ACCEL_X < 543 else
                   1 when ACCEL_X >= 543 and ACCEL_X < 663 else
                   2 when ACCEL_X >= 663 and ACCEL_X < 783 else
                   3 when ACCEL_X >= 783 and ACCEL_X < 903 else
                   4 when ACCEL_X >= 903 and ACCEL_X <= 1023;
               
   --Note: the y direction is inverted
               
    yspeedacc <=   4 when ACCEL_Y >= 0 and ACCEL_Y < 119 else
                   3 when ACCEL_Y >= 119 and ACCEL_Y < 239 else
                   2 when ACCEL_Y >= 239 and ACCEL_Y < 359 else
                   1 when ACCEL_Y >= 359 and ACCEL_Y < 479 else
                   0 when ACCEL_Y >= 479 and ACCEL_Y < 543 else
                  -1 when ACCEL_Y >= 543 and ACCEL_Y < 663 else
                  -2 when ACCEL_Y >= 663 and ACCEL_Y < 783 else
                  -3 when ACCEL_Y >= 783 and ACCEL_Y < 903 else
                  -4 when ACCEL_Y >= 903 and ACCEL_Y <= 1023;
              
    update_freqx <= 1666666;
    
    update_freqy <= 1666666;
              
     -- game-manager / collisions                                 
     process(clk, vsync_signal)
     variable xcheck, ycheck : integer:=0;
     begin
         if(rising_edge(vsync_signal) and showing_logo = '0') then
             busy <= '1';
             done <= '0';
         end if;
         
         if (done = '1') then
             busy <= '0';
         end if;
         
         if(rising_edge(clk)) then
             
             CASE State is
                 when ZERO =>
                     if(busy = '1') then
                         State <= A;
                     end if;
                 when A =>
                         addrb <= STD_LOGIC_VECTOR(to_unsigned(check_left_or_right, addrb'length));
                         State <= B;
                 when B =>
                     if(buffer_1 = 1) then
                         buffer_1 <= 0;
                         State <= C;
                         --for xcheck in 0 to 8 loop
                         if(doutb = "000") then -- wall
                             xspeed <= 0;
                             yspeed <= 0;
                         elsif(doutb = "010") then -- arrival
                             xspeed <= start - xpos;
                             yspeed <= start - ypos;
                                 if(level=lev0) then
                                    level <= lev1;
                                 elsif(level=lev1) then
                                    level <= lev2;
                                 else
                                    level <= lev0;
                                 end if;
                             State <= E;
                         elsif(doutb = "011") then -- hole
                            xspeed <= start - xpos;
                            yspeed <= start - ypos;
                            State <= E;
                         elsif(doutb = "100") then -- speed-up
                             if(xspeedacc < 0) then
                                 xspeed <= xspeedacc-1;
                             elsif (xspeedacc > 0) then
                                 xspeed <= xspeedacc+1;
                             end if;
--                             if(yspeedacc < 0) then
--                                 yspeed <= yspeedacc-2;
--                             elsif (yspeedacc > 0) then
--                                 yspeed <= yspeedacc+2;
--                             end if;
--                            State <= E;
                         elsif(doutb = "101") then -- teleport (odd symmetry)
                            xspeed <= (320 - (xpos + xspeedacc))*2;
                            yspeed <= (240 - (ypos + yspeedacc))*2;
                            State <= E;
                         else
                             xspeed <= xspeedacc;
                         end if;
                         --end loop;
                     else
                         buffer_1 <= buffer_1 + 1;
                     end if;
                 
                 when C =>
                     addrb <= STD_LOGIC_VECTOR(to_unsigned(check_up_or_down, addrb'length));
                     State <= D;
                     
                 when D =>
                     if(buffer_2 = 1) then
                         buffer_2 <= 0;
                         if(doutb = "000") then -- wall
                             yspeed <= 0;
                         elsif(doutb = "010") then -- arrival
                             xspeed <= start - xpos;
                             yspeed <= start - ypos;
                             if(level=lev0) then
                                 level <= lev1;
                              elsif(level=lev1) then
                                 level <= lev2;
                              else
                                 level <= lev0;
                              end if;
                         elsif(doutb = "011") then -- hole
                              xspeed <= start - xpos;
                              yspeed <= start - ypos;
                         elsif(doutb = "100") then -- speed-up
                              if(yspeedacc < 0) then
                                  yspeed <= yspeedacc-1;
                              elsif (yspeedacc > 0) then
                                  yspeed <= yspeedacc+1;
                              end if;
                         elsif(doutb = "101") then -- teleport (odd symmetry)
                              xspeed <= (320 - (xpos + xspeedacc))*2;
                              yspeed <= (240 - (ypos + yspeedacc))*2;
                         else
                             yspeed <= yspeedacc;
                         end if;
                         State <= DB;
                     else 
                         buffer_2 <= buffer_2 + 1;
                     end if;
                     
                 when DB =>
                     State <= E;
                     if(xspeed /= 0 and yspeed /= 0) then
                         addrb <= STD_LOGIC_VECTOR(to_unsigned(check_diagonal, addrb'length));
                         State <= DC;
                     end if;
                     
                 when DC =>
                     if(buffer_4 = 1) then
                        buffer_4 <= 0;
                        State <= E;
                        if(doutb = "000") then
                            xspeed <= 0;
                            yspeed <= 0;
                        end if;
                     else
                        buffer_4 <= buffer_4 + 1;
                     end if;
                     
                 when E =>
                     if(update_accx >= update_freqx) then    
                         update_accx <= 0;
                         xpos <= xpos + xspeed;
                         State <= F;
                     else 
                         update_accx <= update_accx + 1;
                     end if;
                     
                     if(update_accy >= update_freqy) then    
                          update_accy <= 0;
                          ypos <= ypos + yspeed;
                          State <= F;
                      else 
                          update_accy <= update_accy + 1;
                      end if;
                      

                      
                 when F =>
                     if(reset = '1') then
                        addrb <= (others => '0');
                     end if;
                     done <= '1';
                     State <= ZERO;
                 when others =>
                     State <= ZERO;
             END CASE;
                  
         end if;
         
     end process;
     
    
showing_logo <= '0' when logo_counter = show_logo else '1';
reset <= '1' when doutb = "010" else '0';

 
     
process(clk, reset)
begin   
    if(rising_edge(clk)) then
        if(reset = '1') then
            logo_counter <= 0;
            inner_logo_counter <= 0;
        else
            if(logo_counter = show_logo) then
                logo_counter <= logo_counter;
            else
                if(inner_logo_counter = 1666666) then
                    logo_counter <= logo_counter + 1;
                    inner_logo_counter <= 0;
                else
                    inner_logo_counter <= inner_logo_counter + 1;
                end if; 
            end if;
        end if;
    end if;

end process;



end Behavioral;
