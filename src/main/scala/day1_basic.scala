/*
Part 1.

The jungle must be too overgrown and difficult to navigate in vehicles or access from the air; the Elves' expedition traditionally goes on foot. As your boats approach land, the Elves begin taking inventory of their supplies. One important consideration is food - in particular, the number of Calories each Elf is carrying (your puzzle input).

The Elves take turns writing down the number of Calories contained by the various meals, snacks, rations, etc. that they've brought with them, one item per line. Each Elf separates their own inventory from the previous Elf's inventory (if any) by a blank line.

For example, suppose the Elves finish writing their items' Calories and end up with the following list:

1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
This list represents the Calories of the food carried by five Elves:

The first Elf is carrying food with 1000, 2000, and 3000 Calories, a total of 6000 Calories.
The second Elf is carrying one food item with 4000 Calories.
The third Elf is carrying food with 5000 and 6000 Calories, a total of 11000 Calories.
The fourth Elf is carrying food with 7000, 8000, and 9000 Calories, a total of 24000 Calories.
The fifth Elf is carrying one food item with 10000 Calories.
In case the Elves get hungry and need extra snacks, they need to know which Elf to ask: they'd like to know how many Calories are being carried by the Elf carrying the most Calories. In the example above, this is 24000 (carried by the fourth Elf).

Find the Elf carrying the most Calories. How many total Calories is that Elf carrying?
*/
/*
Part 2.

By the time you calculate the answer to the Elves' question, they've already realized that the Elf carrying the most Calories of food might eventually run out of snacks.

To avoid this unacceptable situation, the Elves would instead like to know the total Calories carried by the top three Elves carrying the most Calories. That way, even if one of those Elves runs out of snacks, they still have two backups.

In the example above, the top three Elves are the fourth Elf (with 24000 Calories), then the third Elf (with 11000 Calories), then the fifth Elf (with 10000 Calories). The sum of the Calories carried by these three elves is 45000.

Find the top three Elves carrying the most Calories. How many Calories are those Elves carrying in total?
*/

package day1_basic

import scala.io.Source
import scala.util.Using


def low_level_max(it: Iterator[String]): Int =
  var supr = 0
  var curr = 0

  for line <- it do
    if line == "" then
      supr = curr max supr
      curr = 0
    else
      curr += line.toInt

  curr max supr


def low_level_top(it: Iterator[String], n: Int): Int =
  val top = collection.mutable.ArrayDeque.fill(n)(0)
  var cur = 0

  def update_top(i: Int = 0): Unit =
    if i < n && cur > top(i) then update_top(i + 1)
    else if i > 0 then
      top.insert(i, cur)
      top.dropInPlace(1)

  for line <- it do
    if line == "" then
      update_top()
      cur = 0
    else
      cur += line.toInt
  update_top()

  top.sum


@main def run =
  Using(Source.fromFile("src/main/resources/day1_data.txt"))(f =>
    println(low_level_top(f.getLines(), 3))
  ).get