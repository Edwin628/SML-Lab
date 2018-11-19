structure CardProblem =
struct
open CardType
exception NYI

val held_cards = []
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
		fun remove_cards ([],c,e) = raise e
				|remove_cards(card::cs,c,e) = 
				if card = c then cs
				else card :: remove_cards(cs,c,e)
		(*#4 compare the card color *)
		fun all_same_color (_::[]) = true
			|all_same_color(card1::card2::cards) =
				if card_color(card1) = card_color(card2) then all_same_color(card2::cards)
				else false
		(*#5 caculate the sum of the cards values *)
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
(*fun officiate (_,[],goal) = score(held_cards,goal)
	|officiate(card::cardlist : card list,move::movelist,goal) =*) 
    (*raise NYI(*replace this line with your solution*)*)
fun officiate(card::cardlist : card list,move::movelist,goal) =
	if move=[] then score(held_cards,goal)
	else
	let

		fun trans (Discard x)=x
		val held_cards =
		if move = Draw then card::held_cards
		else  remove_cards(held_cards,trans(move),IllegalMove)
	in
	(*#Q Discard 的用法 *)
		if sum_cards(held_cards) > goal then score(held_cards,goal)
		else officiate(cardlist,movelist,goal)
	end				
end