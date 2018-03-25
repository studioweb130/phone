# Phone Company

Each day at The Phone Company a batch job puts all the customer calls for the previous day into a single log file of:

`'customer id','phone number called','call duration'`

For a customer the cost of a call under 3 minutes is 0.05p/sec, over 3 minutes it is 0.03p/sec. However, there is a promotion on and the calls made to the phone number with the greatest total cost is removed from the customer's bill.

## Task

Write a program that when run will parse the `calls.log` file and print out the total cost of calls for the day for each customer. You can use any libraries you wish to.
