structure Tests =
struct
open CardType
  val tests =  [
    ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15),
    ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42),
    ([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42)
  ]
(*#1 takes a card and returns its color *)
fun card_color sut = 
	case sut of (Spades,_) => Black
	|(Clubs,_) => Black
	|(Diamonds,_) => Red
	|(Hearts,_) => Red
(*#2 takes a card and returns its name *)
fun card_value rnk =
    case rnk of
        (_,Num n) => n
       |(_,Ace) => 11
       |_ => 10
(*#3 remove the cards from the lsit *)
fun remove_cards ([],c) = raise IllegalMove
    |remove_cards(card::cs,c) = 
		if card = c then cs
			else card :: remove_cards(cs,c)
(*#4 compare the card color *)
fun all_same_color (_::[]) = true
	|all_same_color(card1::card2::cards) =
		if card_color(card1) = card_color(card2) then all_same_color(card2::cards)
			else false
(*#5 caculate the sum of the cards values *)
(*#Q 这个sum会不会有记忆，重复使用会不会有后果 *)
val sum = 0
fun sum_cards [] = 0
	|sum_cards(card::cards) = 
	let 
		fun sumcard (card) = sum + card_value(card)
	in
		sumcard(card) + sum_cards(cards)
	end
(*#6 compute the final score *)
fun score (cards,goal) = 
	let 
		val sum = sum_cards(cards)
		val prescore=
		if sum > goal then 3*(sum-goal)
		else goal - sum;
		val scorex = 
		if all_same_color(cards) then prescore div 2
		else prescore
	in
		scorex
	end
end
 