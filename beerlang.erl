-module(beerlang).
-export([start/0]).
-import(timer,[sleep/1]).




start() ->
  BouncerPid = spawn(fun() -> bouncer() end),
  BarPid = spawn(fun() -> bar(#{}, true, BouncerPid) end),
  BTPid = spawn(fun() -> bartender(BarPid) end),
  io:fwrite("the Erlang bar is open, folks!\n"),
  BarPid ! {cus_count_inc, spawn(fun() -> customer(BTPid, beer, "student") end)},
  timer:sleep(300),
  BarPid ! {cus_count_inc, spawn(fun() -> customer(BTPid, wine, "woman") end)},
  timer:sleep(300),
  BarPid ! {cus_count_inc, spawn(fun() -> customer(BTPid, scotch, "drunk") end)}.


bar(Customers, Init, Bouncer) ->
  if (Customers == #{}) and (Init == false) -> io:fwrite("Everyone has gone home - the Erlang bar is closed.\n - THE END -\n");
  true ->
    receive
      {cus_count_inc, CPid} -> bar(add_customer(CPid, Customers), false, Bouncer);
      {cus_count_dec, CPid} -> bar(rm_customer(CPid, Customers), false, Bouncer);
      {cus_units_inc, CPid} -> bar(inc_units(CPid, Customers), false, Bouncer);
      {order_verify, CPid, BTPid, Drink} ->
        Units = get_units(CPid, Customers),
        if
          Units < 1 -> BTPid ! {order_ok, Drink, CPid}, bar(Customers, CPid, Bouncer);
          true ->
            BTPid ! {order_notok, CPid, Drink},
            Bouncer ! {evict, CPid, self()},
            bar(Customers, false, Bouncer)
        end;
      {evict, Pid} -> Bouncer ! {evict, Pid}, bar(Customers, false, Bouncer);
      {evicted, CPid} -> bar(rm_customer(CPid, Customers), false, Bouncer)
    end
  end.

bartender(BarPid) ->
  receive
    {order, Drink, Pid} ->
      BarPid ! {order_verify, Pid, self(), Drink},
      bartender(BarPid);
    {order_ok, beer, Pid} ->
      io:fwrite("bartender: here you go, lad, one beer.\n"),
      Pid ! {order_ok, beer},
      BarPid ! {cus_units_inc, Pid},
      bartender(BarPid);
    {order_ok, wine, Pid} ->
      io:fwrite("bartender: ah, enchantè, we have a lovely Chardonnay. Here you go. \n"),
      Pid ! {order_ok, wine},
      BarPid ! {cus_units_inc, Pid},
      bartender(BarPid);
    {order_ok, scotch, Pid} ->
      io:fwrite("bartender: okay, but I'm keeping an eye on you, pal - one scotch for you.\n"),
      Pid ! {order_ok, scotch},
      BarPid ! {cus_units_inc, Pid},
      bartender(BarPid);
    {order_notok, Pid, Drink} ->
      case Drink of
        beer -> io:fwrite("bartender: sorry, no more beer for you, son\n");
        wine -> io:fwrite("bartender:sorry, madame - you come fo a bit tipsy, Call it a night, perhaps?\n");
        scotch -> io:fwrite("bartender: enough, get outta here, boozer. This is a respectable place\n");
        water -> io:fwrite("bartender: we dont make any money of water. You best get out of here\n")
      end,
      BarPid ! {evict, Pid},
      Pid ! {not_served, Drink},
      bartender(BarPid)
  end.

bouncer() ->
  receive
    {evict, Pid, BPid} ->
      Pid ! {evict, Pid, self()},
      io:fwrite("bouncer: You have had to much to drink, I will have to ask you to leave\n"),
      bouncer(BPid)
  end.

bouncer(BPid) ->
  receive
    {evict, Pid, BPid} ->
      Pid ! {evict, Pid, self()},
      io:fwrite("bouncer: You have had to much to drink, I will have to ask you to leave\n"),
      bouncer(BPid);
    {leaving, Pid} ->
      io:fwrite("bouncer: good riddance, another customer gone\n"),
      BPid ! {evicted, Pid},
      bouncer(BPid)
  end.

customer(BT, Drink, Name) ->
  io:fwrite("~s: One ~s, please ?\n", [Name, Drink]),
  BT ! {order, Drink, self()},
  receive
    {order_ok, beer} ->
      io:fwrite("~s: ah, thanks, mate.\n", [Name]),
      timer:sleep(rand_wait()),
      io:fwrite("~s: ok, time for another one of those crazy beers.\n", [Name]),
      customer(BT, beer, Name);
    {order_ok, wine} ->
      io:fwrite("~s: Wonderful! Oh, it has a nice bouqet.\n", [Name]),
      timer:sleep(rand_wait()),
      io:fwrite("~s: Oh, I think I will have one more tiny glass of wine, and then off to bed.\n", [Name]),
      customer(BT, wine, Name);
    {order_ok, scotch} ->
      io:fwrite("~s: Arrh, keep your nose to yourself and pour it, mister.\n", [Name]),
      timer:sleep(rand_wait()),
      io:fwrite("~s: Arrh, need a drink in the other leg as well.\n", [Name]),
      customer(BT, scotch, Name);
    {not_served, Drink} ->
      case Drink of
        beer -> io:fwrite("~s: huh!? Ok, jolly good, off to the next bar then...\n", [Name]);
        wine -> io:fwrite("~s: That is outragous. I have only had a couple of tiny glasses. Let me speak to the manager. You shall hear from my husband, he is a lawyer.\n", [Name]);
        scotch -> io:fwrite("~s: erfhh...Imaz schober as a donky, pal. I will mess you up bad if you dont pour me another one\n", [Name]);
        water -> io:fwrite("~s: I'll be fine in a minute\n", [Name])
      end,
      timer:sleep(rand_wait()),
      customer(BT, water, Name);
    {evict, Pid, BCPid} -> io:fwrite("~s is leaving the bar\n", [Name]), BCPid ! {leaving, Pid}
  end.

add_customer(Customer, Customers) ->
  Found = maps:find(Customer, Customers),
  if
    Found == error ->
      maps:merge(#{Customer => 0}, Customers);
    true -> Customers
  end.

rm_customer(Customer, Customers) ->
  Found = maps:find(Customer, Customers),
  if
    Found /= error -> maps:remove(Customer, Customers);
    true -> Customers
  end.

inc_units(Customer, Customers) ->
  Found = maps:find(Customer, Customers),
  if
    Found == error -> io:fwrite("can not update inc for ~w", [Customer]), Customers;
    true ->  maps:update(Customer, maps:get(Customer, Customers) + 1, Customers)
  end.

get_units(Customer, Customers) ->
  Found = maps:find(Customer, Customers),
  if
    Found /= error -> maps:get(Customer, Customers);
    true -> 0
  end.

rand_wait() ->
  rand:uniform(10) * 1000.

print_custs(Customers) ->
  maps:fold(
    fun(K, V, ok) ->
      io:fwrite("~p: ~p ", [K, V])
    end, ok, Customers),
  io:fwrite("\n").








