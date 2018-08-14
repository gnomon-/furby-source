import sys

chars = [ "ÉÍ»²È¼ÚÄ¿³ÀÙ", "╔═╗║╚╝┌─┐│└┘" ]

with open( sys.argv[1], "r", encoding = "utf8") as file_in:
	result = file_in.read()

i = result.find( "Í" ) < 0
j = 1 - i

for k in range( 0, 12 ):
	print( chars[i][k] + " to " + chars[j][k] )
	result = result.replace( chars[i][k], chars[j][k] )

with open( sys.argv[1], "w", encoding = "utf8") as file_out:
	file_out.write( result )