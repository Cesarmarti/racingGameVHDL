library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.std_logic_unsigned.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity mem is
    Port ( clk_i 		: in  STD_LOGIC;
           we_i 		: in  STD_LOGIC;
           addrOUT_i : in  STD_LOGIC_VECTOR (6 downto 0);
           data_o 	: out  STD_LOGIC_VECTOR (0 to 219);
			  
			  addrOUTTest : in  STD_LOGIC_VECTOR (6 downto 0);
           data_oTest 	: out  STD_LOGIC_VECTOR (0 to 219));
end mem;

architecture Behavioral of mem is

	type ram_type is array (99 downto 0) of std_logic_vector (0 to 219);
   signal RAM : ram_type:= (
									0 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
1 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
2 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
3 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
4 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
5 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
6 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
7 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
8 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
9 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
10 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
11 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
12 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
13 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
14 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
15 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
16 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
17 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
18 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
19 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
20 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
21 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
22 => "1111111111111111111111111110000000000111111111111111111111111111111111111111111111111111111111111111111111110000000000000000000000000000000000000000000000000000000000000000000000000000000111111111111111111111111111111111",
23 => "1111111111111111111111111000000000000001111111111111111111111111111111111111111111111111111111111111111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000001111111111111111111111111111",
24 => "1111111111111111111111110000000000000000111111111111111111111111111111111111111111111111111111111111111100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011111111111111111111111111",
25 => "1111111111111111111111000001111111111000011111111111111111111111111111111111111111111111111111111111111000001111111111111111111111111111111111111111111111111111111111111111111111111111111000000000111111111111111111111111",
26 => "1111111111111111111110000011111111111100001111111111111111111111111111111111111111111111111111111111110000111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000001111111111111111111111",
27 => "1111111111111111111000001111111111111110001111111111111111111111111111111111111111111111111111111111110001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111100000111111111111111111111",
28 => "1111111111111111110000011111111111111111000111111111111110000000000000111111111111111111111111111111100011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111000001111111111111111111",
29 => "1111111111111111100001111111111111111111000011111111110000000000000000000111111111111111111111111111000011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111100000111111111111111111",
30 => "1111111111111111000011111111111111111111100001111111000000000000000000000001111111111111111111111111000111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111000111111111111111111",
31 => "1111111111111110000111111111111111111111110000111000000001111111111111000000111111111111111111111110001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111100011111111111111111",
32 => "1111111111111110001111111111111111111111111000000000001111111111111111111000011111111111111111111100001111111111111000000000000000000000000000000000000000000000000000000000000000001111111111111111111100001111111111111111",
33 => "1111111111111100011111111111111111111111111100000000111111111111111111111100001111111111111111111100011111111111100000000000000000000000000000000000000000000000000000000000000000000000011111111111111110001111111111111111",
34 => "1111111111111100011111111111100011111111111111000111111111111111111111111110001111111111111111111000111111111111000000000000000000000000000000000000000000000000000000000000000000000000000011111111111111000111111111111111",
35 => "1111111111111100011111111111000001111111111111111111111111111111111111111111000111111111111111110000111111111110000111111111111111111111111111111111100000000000000000011111111111110000000001111111111111000111111111111111",
36 => "1111111111111000111111111110000001111111111111111111111111111111111111111111000111111111111111100001111111111110001111111111111111111111111111111100000000000000000000000011111111111111100000011111111111100011111111111111",
37 => "1111111111111000111111111100001000111111111111111111111111111111111111111111000111111111111111100011111111111100011110000000000001111111111111110000000000000000000000000000011111111111111000001111111111100011111111111111",
38 => "1111111111111000111111111000011000011111111111111111111111111111111111111111000111111111111111000111111111111000011000000000000000011111111111000000011111111111111111100000000111111111111110000111111111100011111111111111",
39 => "1111111111111000111111111000111100000111111111111111111111111000001111111111100011111111111110000111111111111000100000000000000000001111111110000011111111111111111111111100000000111111111111000011111111100011111111111111",
40 => "1111111111111000111111111000111110000011111111111111111111000000000111111111100011111111111100001111111111110000000001111111111110000111111100001111111111111111111111111111100000001111111111100011111111100011111111111111",
41 => "1111111111111000111111111000111111100001111111111111111100000000000011111111100011111111111000011111111111100000000111111111111111000011111000011111111111111111111111111111111000000001111111110001111111100011111111111111",
42 => "1111111111111000111111111000111111110000111111111111110000000111100001111111100011111111110000111111111111000000011111111111111111100001111000111111111111111111111111111111111111000000001111110001111111100011111111111111",
43 => "1111111111111100011111111000111111111000000000000000000000111111110001111111110001111111100001111111111111000000111111111111111111110000110001111111111111111111111111111111111111110000000000000001111111100011111111111111",
44 => "1111111111111100011111111100000111111100000000000000000011111111110001111111110001111111000011111111111110000001111111111111111111111000010001111111111111111111111111111111111111111110000000000011111111100011111111111111",
45 => "1111111111111100011111111100000001111111000000000000001111111111111000111111110001111100000111111111111100000011111111111111111111111100000011111111111111100000111111111111111111111111110000001111111111100011111111111111",
46 => "1111111111111100011111111111000000001111111111111111111111111111111000111111111000111000001111111111111100000111111111111111111111111110000011111111111110000000000111111111111111111111111111111111111111100011111111111111",
47 => "1111111111111110001111111111111000000011111111111111111111111111111000111111111000010000111111111111111000001111111111111111111111111110000011111111111000000000000000111111111111111111111111111111111111100011111111111111",
48 => "1111111111111110001111111111111110000000011111111111111111111111111000111111111100000001111111111111110000001111111111111001111111111111000011111111110000011111000000001111111111111111111111111111111111100011111111111111",
49 => "1111111111111110001111111111111111110000000111111111111111111111111100011111111110000011111111111111100000011111111111110000111111111111000011111111100001111111111000000001111111111111111111111111111111100011111111111111",
50 => "1111111111111110001111111111111111111100000001111111111111111111111100011111111111001111111111111111000000111111111111100000011111111111100011111111000011111111111111000000001111111111111111111111111111000111111111111111",
51 => "1111111111111111000111111111111111111111100000011111111111111111111100011111111111111111111111111110000001111111111111000000001111111111100011111111000111111111111111110000000011111111111111111111111110000111111111111111",
52 => "1111111111111111000111111111111111111111111000000011111111111111111100011111111111111111111111111100000001111111111110000110000111111111100011111111000111111111111111111110000000011111111111111111111000001111111111111111",
53 => "1111111111111111100011111111111111111111111110000000111111111111111110001111111111111111111111111000010001111111111100001111000011111111110011111111000111111111111111111111110000000000111111111111000000011111111111111111",
54 => "1111111111111111100000001111111111111111111111100000001111111111111110001111111111111111111111100000100011111111111000011111100011111111110011111111000111111111111111111111111100000000000000000000000001111111111111111111",
55 => "1111111111111111110000000011111111111111111111111100000011111111111111000111111111111111111111000001100011111111111000111111110001111111111011111111000111111111111111111111111111100000000000000000000111111111111111111111",
56 => "1111111111111111111100000000011111111111111111111111000000111111111111000011111111111111111100000111100011111111110001111111110000111111111011111111000111111111111111111111111111111111000000000000111111111111111111111111",
57 => "1111111111111111111111110000000111111111111111111111110000001111111111100001111111111111110000001111000111111111100001111111111000111111111111111111000111111111111111111111111111111111111111111111111111111111111111111111",
58 => "1111111111111111111111111100000000111111111111111111111100000001111111110000011111111100000000111111000111111111000011111111111000111111111111111111000111111111111111111111111111111111111111111111111111111111111111111111",
59 => "1111111111111111111111111111100000001111111111111111111111000000011111111000000000000000000011111111000111111111000111111111111000111111111111111111000111111111111111111111111111111111111111111111111111111111111111111111",
60 => "1111111111111111111111111111111000000001111111111111111111110000000011111110000000000000001111111110001111111111000111111111111100011111111111111111000111111111111111111111111111111111111111111111111111111111111111111111",
61 => "1111111111111111111111111111111111000000011111111111111111111110000000111111100000000011111111111110001111111110001111111111111100011111111111111111000111111111111111111111111111111111111111111111111111111111111111111111",
62 => "1111111111111111111111111111111111110000000111111111111111111111100000001111111111111111111111111100011111111110001111111111111110001111111111111111000111111111111111111111111111111111111111111111111111111111111111111111",
63 => "1111111111111111111111111111111111111110000001111111111111111111111100000011111111111111111111111000011111111110001111111111111110001111111111111111000111111111111111111111111111111111111111111111111111111111111111111111",
64 => "1111111111111111111111111111111111111111100000011111111111111111111111000000011111111111111111110000111111111100011111111111111111000111111111111110001111111111111111111111111111111111111111111111111111111111111111111111",
65 => "1111111111111111111111111111111111111111111000000111111111111111111111110000000111111111111111100001111111111100011111111111111111000111111111111110001111111111111111111111111111111111111111111111111111111111111111111111",
66 => "1111111111111111111111111111111111111111111110000001111111111111111111111100000000111111111110000011111111111100011111111111111111100011111111111110001111111111111111111111111111111111111111111111111111111111111111111111",
67 => "1111111111111111111111111111111111111111111111100000001111111111111111111111100000000011111000000111111111111000111111111111111111100001111111111100011111111111111111111111111111111111111111111111111111111111111111111111",
68 => "1111111111111111111111111111111111111111111111111000000011111111111111111111111000000000000000011111111111111000111111111111111111110000000000000000011111111111111111111111111111111111111111111111111111111111111111111111",
69 => "1111111111111111111111111111111111111111111111111110000000011111111111111111111111000000000001111111111111110001111111111111111111111000000000000000111111111111111111111111111111111111111111111111111111111111111111111111",
70 => "1111111111111111111111111111111111111111111111111111110000000111111111111111111111111100000111111111111111110001111111111111111111111110000000000011111111111111111111111111111111111111111111111111111111111111111111111111",
71 => "1111111111111111111111111111111111111111111111111111111100000000111111111111111111111111111111111111111111100011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
72 => "1111111111111111111111111111111111111111111111111111111111100000011111111111111111111111111111111111111111000011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
73 => "1111111111111111111111111111111111111111111111111111111111111000000011111111111111111111111111111111111110000111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
74 => "1111111111111111111111111111111111111111111111111111111111111110000000111111111111111111111111111111111000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
75 => "1111111111111111111111111111111111111111111111111111111111111111100000001111111111111111111111111111110000011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
76 => "1111111111111111111111111111111111111111111111111111111111111111111100000001111111111111111111111111000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
77 => "1111111111111111111111111111111111111111111111111111111111111111111111000000000111111111111111111100000011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
78 => "1111111111111111111111111111111111111111111111111111111111111111111111110000000000000000000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
79 => "1111111111111111111111111111111111111111111111111111111111111111111111111110000000000000000000000000111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
80 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111000000000000000000011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
81 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
82 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
83 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
84 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
85 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
86 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
87 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
88 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
89 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
90 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
91 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
92 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
93 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
94 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
95 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
96 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
97 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
98 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
99 => "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",


                            others => (others => '0'));
	signal dataOUT : STD_LOGIC_VECTOR (0 to 219);
	signal dataOUTTest : STD_LOGIC_VECTOR (0 to 219);
begin
	data_o	<= dataOUT;
	data_oTest	<= dataOUTTest;

	-- to je dvokanalni RAM. Pisemo na naslov addrIN_i, istocasno lahko beremo z naslova addrOUT_i
	-- RAM ima asinhronski bralni dostop, tako da ga je easy za uporabit. Ko naslovis, takoj dobis podatke.
	-- pisalni dostop je sinhronski.
	-- Pazi LSB bit je skrajno levi, zato da se lazje 'ujema' z organizacijo zaslona!

	
	dataOUTTest <= RAM(conv_integer(addrOUTTest));
	dataOUT <= RAM(conv_integer(addrOUT_i));


end Behavioral;
