# ProLog

This directory contains a ProLog script for converting a valid boolean expression is its equivalent Disjunctive Normal Form (DNF).

Check the [documentation](#documentation) to know the semantics and the [examples](#examples) to know the syntax.

## Documentation

`dnf(E, F)` is `true` if and only if `F` is the DNF of expression `E`.

## Usage

To use this script, first consult it in your prolog environment:

```bash
swipl disjunctive_normal_form.pl
```

## Examples

- Querying
  
  ```prolog
  dnf(-(a+b), E).
  ```
  
  gives
  
  ```text
  E = -a * -b
  ```

- Querying
  
  ```prolog
  dnf(x*(a+b), E).
  ```

  gives
  
  ```text
  E = x*a + x*b
  ```

- Querying
  
  ```prolog
  dnf(- -x * (- -a + -b * (c + -d)), E).
  ```
  
  gives
  
  ```text
  E = x*a + (x * (-b * c) + x* (-b * -d))
  ```
