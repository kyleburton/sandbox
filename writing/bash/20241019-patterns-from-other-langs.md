I have found great benefits from learning other programming langauges.  Clojure helped me better understand how to leverage persistent immutable data strucures and level up my referential transparency chops which led to more robust and more re-useable code.  Rust helped me improve my understanding of ownership and memory management - making languages like Java and Go look dangerous yet improved how I managed resources in these langauges.

Erlang is another one of those langauges that brought concepts that improved my programming.  Erlang was one of the first languages where I was introduced the wide spread use of case statements, crucially where there was no default or fall-through.  The programmer could not fail to remember to handle a case, as the statement would "crash".

Erlang's case statemnts also make use of tuples to transform what would be a cascade of if/else statements into a declarative "switch" statement.


```erlang
case {AccountStatus, AllItemsInStock} of
    {active_account, in_stock}      -> purchase_and_ship(Customer, Items);
    {active_account, out_of_stock}  -> place_backorder(Customer, Items);
    {active_account, _}             -> place_pending_order(Customer, Items);
    {no_account,     in_stock}      -> send_to_guest_checkout(Customer, Items);
    {no_account,     _}             -> send_to_please_come_back_later_screen(Customer, Items)
end.
```

I like this pattern, it is concise.  When you have multiple conditions it becomes pretty clear which conditions are correlated together.

This logic in bash using if statements is a lot less compact and concise:

```bash
if [[ "$account_status" == "active_account" && "$all_items_in_stock" == "in_stock" ]]; then
  purchase_and_ship "$customer" "$items"
fi

if [[ "$account_status" == "active_account" && "$all_items_in_stock" == "out_of_stock" ]]; then
  place_backorder "$customer" "$items"
fi

if [[ "$account_status" == "active_account" ]]; then
  place_pending_order "$customer" "$items"
fi

if [[ "$account_status" == "no_account" && "$all_items_in_stock" == "in_stock" ]]; then
  send_to_guest_checkout "$customer" "$items"
fi

if [[ "$account_status" == "no_account" ]]; then
  send_to_please_come_back_later_screen "$customer" "$items"
fi
```

If we are willing to concatenate our "tuple" into a single string, we can use bash's `case` statement to achieve a similar outcome:

```bash
case "$account_status|$all_items_in_stock" in
  active_account|in_stock)     purchase_and_ship                         "$customer" "$items" ;;
  active_account|out_of_stock) place_backorder                           "$customer" "$items" ;;
  active_account|*)            place_pending_order                       "$customer" "$items" ;;
  no_account|in_stock)         send_to_guest_checkout                    "$customer" "$items" ;;
  no_account|*)                send_to_please_come_back_later_screen     "$customer" "$items" ;;
esac
```

Chosing a `|` as a delimiter is a compromise - it is hopefully unlikley to appear in our arguments.
