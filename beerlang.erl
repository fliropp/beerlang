-module(beerlang).
-export([start/0]).
-import(timer,[sleep/1]).

start() ->
  BarPid = spawn(fun() -> bar([], true) end),
  BTPid = spawn(fun() -> bartender(BarPid) end),
  io:fwrite("the Erlang bar is open, folks!\n"),
  BarPid ! {cus_count_inc, spawn(fun() -> customer(BTPid, beer, 0) end)},
  timer:sleep(300),
  BarPid ! {cus_count_inc, spawn(fun() -> customer(BTPid, wine, 0) end)},
  timer:sleep(300),
  BarPid ! {cus_count_inc, spawn(fun() -> customer(BTPid, scotch, 0) end)}.


bar(Customers, Init) ->
  if (Customers == []) and (Init == false) -> io:fwrite("Everyone has gone home - the Erlang bar is closed.\n - THE END -\n");
  true ->
    receive
      {cus_count_inc, CPid} -> bar(lists:append(Customers, [CPid]), false);
      {cus_count_dec, CPid} -> bar(lists:delete(CPid, Customers), false);
      print_bar_status -> io:fwrite("---\ncusts in bar: ~p ~n\n----\n", [Customers]), bar(Customers, false)
    end
  end.

bartender(BarPid) ->
  receive
    {order, Drink, Pid, Count } when Count > 1->
      io:fwrite("Sorry, you have had enough. Go home and get some sleep.\n"),
      Pid ! {not_served, Drink},
      BarPid ! {cus_count_dec, Pid},
      bartender(BarPid);
    {order, beer, Pid, Count } ->
      io:fwrite("here you go, lad, one beer.\n"),
      Pid ! {beer, 5678, Count + 1},
      bartender(BarPid);
    {order, wine, Pid, Count } ->
      io:fwrite("ah, enchantè, we have a lovely Chardonnay. Here you go. \n"),
      Pid ! {wine, 2567, Count + 1},
      bartender(BarPid);
    {order, scotch, Pid , Count} ->
      io:fwrite("okay, but I'm keeping an eye on you, pal - one scotch for you.\n"),
      Pid ! {scotch, 4123, Count + 1},
      bartender(BarPid)
  end.

customer(B, Drink, InitCount) ->
  io:fwrite("One ~s, please ?\n", [Drink]),
  B ! {order, Drink, self(), InitCount},
  receive
    {beer, Size, Count} ->
      io:fwrite("ah, thanks, mate.\n"),
      timer:sleep(Size ),
      io:fwrite("ok, time for another one of those crazy beers.\n"),
      customer(B, beer, Count);
    {wine, Size, Count} ->
      io:fwrite("Wonderful! Oh, it has a nice bouqet.\n"),
      timer:sleep(Size),
      io:fwrite("Oh, I think I will have one more tiny glass of wine, and then off to bed.\n"),
      customer(B, wine, Count);
    {scotch, Size, Count} ->
      io:fwrite("Arrh, keep your nose to yourself and pour it, mister.\n"),
      timer:sleep(Size),
      io:fwrite("Arrh, need a drink in the other leg as well.\n"),
      customer(B, scotch, Count);
    {not_served, beer} -> io:fwrite("(huh!? Ok, off to the next bar then...)\n");
    {not_served, wine} -> io:fwrite("That is outragous. I have only had a couple of tiny glasses. Let me speak to the manager. You shall hear from my husband, he is a lawyer.\n");
    {not_served, scotch} -> io:fwrite("erfhh...Imaz schober as a donky, pal. I will mess you up bad if you dont pour me another one\n")
  end.




