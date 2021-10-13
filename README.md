# PostgreSQL API language search


## PostgreSQL API functions response:

We don't use PostgreSQL in the usual way.

We do everything through API functions, which always return two values:

1. "ok" boolean to say whether API call was successful
2. "js" JSON response, whether as intended or with just one key: "error"

So we only need a low-level PostgreSQL library to access this.


## PostgreSQL API test table and functions

One table, "things", with two sample rows inserted for testing.

Four functions with four different parameters:

1. things()
2. thing\_get(integer)
3. thing\_add(text, text)
4. thing\_rename(integer, text)

See the file **postgresql.sql** here.


## API client usage

The API client should:

1. take the function name
2. … and variable arguments of different types
3. returning the "ok" boolean and "js" parsed JSON as a hash/map/array

### API client example:

````
ok, js = qa("things")
ok, js = qa("thing_get", 1)
ok, js = qa("thing_add", "new name", "category")
ok, js = qa("thing_rename", 2, "new name")
````


## API client, behind the scenes:

The API client needs to convert a call into SQL, like this:

First, count the incoming arguments to make parameter string

* no arguments = "()"
* one argument = "($1)"
* two arguments = "($1, $2)"
* three arguments = "($1, $2, $3)"
* … and so on

Then, combine the function name to make the SQL string with numbered parameters.  To match the above API client examples:

````
"select ok, js from things()"
"select ok, js from thing_get($1)"
"select ok, js from thing_add($1, $2)"
"select ok, js from thing_rename($1, $2)"
````

Then, pass that to PostgreSQL's "exec_params" with the actual arguments in an array.

Then, parse the PostgreSQL response so that:

1. "t" or "f" is converted to native boolean true or false
2. JSON is decoded into a hash/map or array, whose values can be string (UTF-8), integer, float, boolean, null, hash/map, or array

Dates do not need to be converted to native date type. They can remain strings.


# YOUR MISSION: Do this in your language

See my Ruby example here: **ruby-test.rb**

I'd like to see how this can be done in your language.

First, install PostgreSQL and Ruby with the **-init.sh** scripts here.

Then, run "ruby test-ruby.rb" to make sure my example test works for you.

Then, make your own in your language.


## GOALS:

### 1. simple

Don't be complex and abstract.

Make it as simple as possible.  This is just a proof of concept for a tiny app.  Not a massive enterprise.

My Ruby API client is only 5 lines, plus 1 line to connect to the database.

Yours will probably be longer, but not much longer.

### 2. minimal included modules

Use whatever is built-in to your language as much as possible, for easier deployment and management.

My Ruby API client has only one requirement: 'pg'

### 3. generic 

Although the tiny example here only has one database table and four functions, the real world usage of this has hundreds of functions with various responses.

So your API must be able to pass back any JSON response, not try to model responses with a Struct.

Make one generic function, as described in "API client, behind the scenes", above.  Don't make separate API functions for the four PostgreSQL functions given here.

Again: this one API function has to work for hundreds of different PostgreSQL functions and JSON responses.

### 4. init script and unit tests as proof

Besides the (Linux/BSD) OS-level installing of your language, make an init script (to be run as superuser) to install any libraries your example needs.

My example here, **ruby-init.sh**, is just one line: "gem install pg".
Then the unit tests can be run as "ruby test-ruby.rb". 
Yours should be equally simple.

Re-create the seven tests I have in **test-ruby.rb**.

No need for a fancy testing suite.  Note my one "assert\_equal" function in Ruby was enough.

