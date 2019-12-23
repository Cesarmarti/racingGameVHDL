----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    13:52:10 11/18/2019 
-- Design Name: 
-- Module Name:    hsync - Behavioral 
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

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity hsync is
    Port ( clk : in  STD_LOGIC;
           reset : in  STD_LOGIC;
           hsync : out  STD_LOGIC;
           column : out  STD_LOGIC_VECTOR (9 downto 0);
           rowclk : out  STD_LOGIC;
           hvidon : out  STD_LOGIC);
end hsync;

architecture Behavioral of hsync is
signal enable : std_logic;
signal countPre : STD_LOGIC_VECTOR (1 downto 0) := (others => '0');
signal count : STD_LOGIC_VECTOR (9 downto 0) := (others => '0');

begin

	column <= count;
	--prescaler
	process(clk)
	begin
		if (clk'event and clk = '1') then
			if (reset = '1') then
				countPre <= (others => '0');
			else
				if (countPre = 3) then
					countPre <= (others => '0');
					enable <= '1';
				else
					countPre <= countPre + 1;
					enable <= '0';
				end if;
			end if;
		end if;
	end process;

	--counter
	process(clk)
	begin
		if (clk'event and clk = '1') then
			if (reset = '1') then
				count <= (others => '0');
			else
				if enable = '1' then
					if (count = 799) then
						count <= (others => '0');
					else
						count <= count + 1;
					end if;
				else
					count <= count;
				end if;
			end if;
		end if;
	end process;
	
	--HVIDON signal
	process(count)
	begin
		if (count < 640) then
			hvidon <= '1';
		else
			hvidon <= '0';
		end if;
	end process;
	
	
	--HSYNC signal
	process(count)
	begin
		if (count > 656 and count < 752) then
			hsync <= '0';
		else
			hsync <= '1';
		end if;
	end process;
	
	--ROWCLK signal
	process(count,enable)
	begin
		if (count = 799 and enable = '1') then
			rowclk <= '1';
		else
			rowclk <= '0';
		end if;
	end process;
end Behavioral;

