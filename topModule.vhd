----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    12:28:03 11/19/2019 
-- Design Name: 
-- Module Name:    topModule - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
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
use IEEE. STD_LOGIC_ARITH. ALL;
use IEEE. STD_LOGIC_UNSIGNED. ALL;
use ieee.numeric_std.all;
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity topModule is
Port ( clk : in  STD_LOGIC;
       reset : in  STD_LOGIC;
		 btnL : in STD_LOGIC;
		 btnR : in STD_LOGIC;
		 btnU : in STD_LOGIC;
		 btnD : in STD_LOGIC;
		 hsync_o : out STD_LOGIC;
		 vsync_o : out STD_LOGIC;
		 vga_r : OUT STD_LOGIC_VECTOR(3 downto 0);
		 vga_g : OUT STD_LOGIC_VECTOR(3 downto 0);
		 vga_b : OUT STD_LOGIC_VECTOR(3 downto 0));
end topModule;

architecture Behavioral of topModule is
signal hvidon : std_logic;
signal vvidon : std_logic;
signal rowclk: STD_LOGIC;
signal column : STD_LOGIC_VECTOR(9 downto 0); 
signal rows : STD_LOGIC_VECTOR(9 downto 0);
signal ramDataO : STD_LOGIC_VECTOR(0 to 219);
signal rowSelectP1 : STD_LOGIC_VECTOR(6 downto 0);

signal ramDataOCheck : STD_LOGIC_VECTOR(0 to 219);
signal rowSelectP1Check : STD_LOGIC_VECTOR(6 downto 0);


--offsetP1 pozicija levega zgornjeva levega kota za P1, posP1X trenutna pozicija pixla ki se izrisuje
-- newOffset, novi offset po premiku, movementP1X, pove sam ce se je v eno smer premaknu
-- check, check2 uporabljena v state machinu za collision detection, isto state pa nextState
--p1POS je pozicija playerja 1(prov zoge), newp1POS je nov pozicija
signal tempX1 : STD_LOGIC_VECTOR(11 downto 0):=(others => '0');
signal tempY1 : STD_LOGIC_VECTOR(11 downto 0):=(others => '0');

signal p1POSX : STD_LOGIC_VECTOR(11 downto 0) := "000101000000";
signal p1POSY : STD_LOGIC_VECTOR(11 downto 0) := "000011110000";
signal newp1POSX : STD_LOGIC_VECTOR(11 downto 0) := "000101000000";
signal newp1POSY : STD_LOGIC_VECTOR(11 downto 0) := "000011110000";
signal offsetP1X : STD_LOGIC_VECTOR(11 downto 0) := (others => '0');
signal offsetP1Y : STD_LOGIC_VECTOR(11 downto 0) := (others => '0');
signal newoffsetP1X : STD_LOGIC_VECTOR(11 downto 0) := (others => '0');
signal newoffsetP1Y : STD_LOGIC_VECTOR(11 downto 0) := (others => '0');
signal posP1X : STD_LOGIC_VECTOR(11 downto 0) := (others => '0');
signal posP1Y : STD_LOGIC_VECTOR(11 downto 0) := (others => '0');
signal movementP1X : std_logic := '0';
signal movementP1Y : std_logic := '0';
signal check : std_logic := '0';
signal check2 : std_logic := '0';
type state_type is (st_CHECK, st_ASSIGN); 
signal state : state_type := st_CHECK; 
signal nextState : state_type := st_CHECK; 

signal cnt_1s : std_logic_vector(27 downto 0) := (others => '0');
signal enableRead : std_logic := '0';
signal colCounter1 : std_logic_vector(2 downto 0) := (others=>'0');
		component hsync is
		Port ( clk : in  STD_LOGIC;
           reset : in  STD_LOGIC;
           hsync : out  STD_LOGIC;
           column : out  STD_LOGIC_VECTOR (9 downto 0);
           rowclk : out  STD_LOGIC;
           hvidon : out  STD_LOGIC);
		end component;
		
		component vsync is
		Port ( clk : in  STD_LOGIC;
           reset : in  STD_LOGIC;
           rowclk : in  STD_LOGIC;
           vsync : out  STD_LOGIC;
           vvidon : out  STD_LOGIC;
           row : out  STD_LOGIC_VECTOR (9 downto 0));
		end component;
		
		component mem is
		Port (clk_i : in  STD_LOGIC;
           we_i 		: in  STD_LOGIC;
           addrOUT_i : in  STD_LOGIC_VECTOR (6 downto 0);
           data_o 	: out  STD_LOGIC_VECTOR (0 to 219);
			  addrOUTTest : in  STD_LOGIC_VECTOR (6 downto 0);
           data_oTest 	: out  STD_LOGIC_VECTOR (0 to 219));
		end component;

begin
	hs: hsync
	port map(
				clk => clk,
				reset => reset,
				rowclk => rowclk,
				hsync => hsync_o,
				hvidon =>hvidon,
				column => column
				);

	vs : vsync
	port map(
				clk => clk,
				reset => reset,
				rowclk => rowclk,
				vsync => vsync_o,
				vvidon => vvidon,
				row => rows
				);
				
	mm : mem
	port map(
				clk_i => clk,
           we_i 		=>'1',
           addrOUT_i =>rowSelectP1,
           data_o 	=> ramDataO,
			  addrOUTTest =>rowSelectP1Check,
           data_oTest 	=> ramDataOCheck
				);

	process(clk) 
	begin
      if (clk'event and clk = '1') then
         state <= nextState;
      end if;
   end process;

	--state machine narjen za cekiranje ce se lah tja premaknemo
	process(state, check,check2)
	begin
		nextState <= state;
      case (state) is
         when st_CHECK =>
            if check = '1' then
               nextState <= st_ASSIGN;
            end if;
            
         when st_ASSIGN =>
            if check2 = '1' then
               nextState <= st_CHECK;
            end if;
				
			when others =>	
            nextState <= st_CHECK;
      end case;      
   end process;


	--proces za branje inputa playerja 1, naj bo sinhroniziran z uro, k ce ne je vse cudn
	process(clk)
	begin
		if(clk'event and clk = '1') then
			if(btnR = '1' and enableRead = '1') then
				newp1POSX<=p1POSX+5;
				newoffsetP1X<=offsetP1X+5;
				movementP1X<= '1';
			elsif(btnL = '1' and enableRead = '1') then
				newp1POSX<=p1POSX-5;
				newoffsetP1X<=offsetP1X-5;
				movementP1X<= '1';
			else
				movementP1X<= '0';
			end if;
			
			if(btnU = '1' and enableRead = '1') then
				newp1POSY<=p1POSY-5;
				newoffsetP1Y<=offsetP1Y-5;
				movementP1Y<= '1';
			elsif(btnD = '1' and enableRead = '1') then
				newp1POSY<=p1POSY+5;
				newoffsetP1Y<=offsetP1Y+5;
				movementP1Y<= '1';
			else
				movementP1Y<= '0';
			end if;
		end if;
	end process;
	
	
	--proces ki bo zaznal da se avto premika in pogledu ce se lahko v to smer premika in dal signal ok (collision) za zdej sam ok
	process(movementP1X,movementP1Y,clk)
		variable temp : std_logic_vector(11 downto 0);
		variable temp2 : std_logic_vector(11 downto 0);
	begin
		if(clk'event and clk = '1')then			
			if(movementP1X = '1' or movementP1Y = '1') then
				if(colCounter1=0) then
					rowSelectP1Check<=newp1POSY(10 downto 4);
					temp:=std_logic_vector(newp1POSX+12);
					if(ramDataOCheck(conv_integer(temp(11 downto 4))) = '1') then
						temp:=std_logic_vector(newp1POSX-12);
						if(ramDataOCheck(conv_integer(temp(11 downto 4))) = '1')then
							colCounter1<=colCounter1+1;
						else
							colCounter1<=(others => '0');
							check<='0';
						end if;
					else
						colCounter1<=(others => '0');
						check<='0';
					end if;
				elsif (colCounter1=1) then
					temp:=std_logic_vector(newp1POSY-12);
					rowSelectP1Check<=temp(10 downto 4);
					if(ramDataOCheck(conv_integer(newp1POSX(11 downto 4))) = '1') then
						colCounter1<=colCounter1+1;
					else
						colCounter1<=(others => '0');
						check<='0';
					end if;
				elsif(colCounter1=2) then
					temp2:=std_logic_vector(newp1POSY+12);
					rowSelectP1Check<=temp2(10 downto 4);
					if(ramDataOCheck(conv_integer(newp1POSX(11 downto 4))) = '1') then
						colCounter1<=(others => '0');
						check<='1';
					else
						colCounter1<=(others => '0');
						check<='0';
					end if;
				end if;
			else
				check<='0';
			end if;
			
			
		end if;
	end process;
	
	process(vvidon,hvidon)
	begin
		if(vvidon = '0' and hvidon = '0' and state = st_ASSIGN)then
			offsetP1X <= newoffsetP1X;
			offsetP1Y <= newoffsetP1Y;
			p1POSY<= newp1POSY;
			p1POSX<= newp1POSX;
			check2 <= '1';
		else
			check2 <= '0';
		end if;
	end process;
	
	--readEnable signal timer, na vsake 1/20 sekunde, za zdej
	process(clk)
	begin
		if (clk'event and clk = '1') then
			if (reset = '1') then
				cnt_1s <= (others => '0');
			else
				if (cnt_1s = (1000000-1)) then
					cnt_1s <= (others => '0');
					enableRead <= '1';
				else
					cnt_1s <= cnt_1s + 1;
					enableRead <= '0';
				end if;
			end if;
		end if;
	end process;

	process(hvidon,vvidon,rows,column,ramDataO,offsetP1X,posP1X)
	begin
			if (hvidon = '1' and vvidon = '1') then
				--vga_r(1 downto 0) <= column(7 downto 6);
				--vga_r(3 downto 2) <= rows(3 downto 2);
				--vga_b(1 downto 0) <= column(5 downto 4);
				--vga_b(3 downto 2) <= rows(5 downto 4);
				--vga_g(1 downto 0) <= column(3 downto 2);
				--vga_g(3 downto 2) <= rows(7 downto 6);
				posP1Y <=offsetP1Y+rows;
				rowSelectP1<=posP1Y(10 downto 4);
				
				posP1X <= offsetP1X+column;
				if(ramDataO(conv_integer(posP1X(11 downto 4))) = '0') then
					vga_r <= "1111";
					vga_b <= "1111";
					vga_g <= "1111";
				else 
					vga_r <= "0000";
					vga_b <= "0001";
					vga_g <= "0000";
				end if;
				
				--izris zoge za P1, to dej na polovico ekrana pol
				if((conv_integer(column)-320)*(conv_integer(column)-320)+(conv_integer(rows)-240)*(conv_integer(rows)-240)<225)then
					vga_r <= "1110";
					vga_b <= "0000";
					vga_g <= "0000";
				end if;
				
				
				
			else
				vga_r <= "0000";
				vga_b <= "0000";
				vga_g <= "0000";
			end if;
	end process;
end Behavioral;

