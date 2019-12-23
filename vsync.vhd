----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    14:12:54 11/18/2019 
-- Design Name: 
-- Module Name:    vsync - Behavioral 
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

entity vsync is
    Port ( clk : in  STD_LOGIC;
           reset : in  STD_LOGIC;
           rowclk : in  STD_LOGIC;
           vsync : out  STD_LOGIC;
           vvidon : out  STD_LOGIC;
           row : out  STD_LOGIC_VECTOR (9 downto 0));
end vsync;

architecture Behavioral of vsync is
signal count : STD_LOGIC_VECTOR (9 downto 0) := (others => '0');

begin
	row<=count;

	--counter
	process(clk)
	begin
		if (clk'event and clk = '1') then
			if (reset = '1') then
				count <= (others => '0');
			else
				if rowclk = '1' then
					if (count = 520) then
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

	--VVIDON signal
	process(count)
	begin
		if (count < 480) then
			vvidon <= '1';
		else
			vvidon <= '0';
		end if;
	end process;
	
	
	--VSYNC signal
	process(count)
	begin
		if (count > 509 and count < 511) then
			vsync <= '0';
		else
			vsync <= '1';
		end if;
	end process;

end Behavioral;

