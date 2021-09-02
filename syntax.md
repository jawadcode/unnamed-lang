# Syntax Brainstorming

## Hello World:
```
function main = print "Hello World"
```

## Fibonacci:
```
function fib: x =
  if x < 2 then x
  else fib ((x - 2) + (x - 1))

function main do
  print "Fib 32: ";
  printf "{}" {(fib 32)};
end
```

```
function fib: x = match x
  | < 2 => x
  | x   => fib ((x - 2) + (x - 1))

function main do
  print "Fib 32: ";
  printf "{}" {(fib 32)};
end
```

## Enum:
```
enum Colour =
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Indigo
  | Violet

impl ToString on Colour
  function to_string self = match
    | Colour::Red    => "red"
    | Colour::Orange => "orange"
    | Colour::Yellow => "yellow"
    | Colour::Green  => "green"
    | Colour::Blue   => "blue"
    | Colour::Indigo,
      Colour::Violet => "purple"
  end
end

function main do
  let colour = Colour::Red;
  printf "Colour: {}\n" { colour };
end
```

## Sigma:
```
import std::io

function read_int = io::stdin ().map int::from?
function sigma start stop f = (start..stop).map f.sum

function main do
  print "Start: ";
  let start = read_int;

  print "\nStop: ";
  let stop = read_int;

  printf "\nResult: {}" {sigma start stop fn x => x + 1}
end
```

## Structs:
```
struct Person = { name: string, age: int }

impl Person
  function new name age = Self { name, age }
  
  function info self =
    printf "My name is {} and I am {} years old\n" { self.name, self.age }
end

function main do
  let me = Person::new "Jawad" 420;
  me.info;
end
```

## Maybe:
```
enum Maybe[a] =
  | Just: a
  | None

function main do
  let input = io::stdin ();
  match input
  | Just str => println str
  | None     => eprintln "Failed to read input"
  end
end
```