case class Book(title: String, authors: List[String])

val books = Set(Book(title = "1", authors = List("AB", "CD")),
  Book(title = "2", authors = List("XY", "CD")),
  Book(title = "3", authors = List("CD", "MN")),
  Book(title = "3", authors = List("XY", "ZZ", "MN"))
)

// authors with at least 2 books
for {
  b1 <- books
  b2 <- books
  if b1.title < b2.title
  a1 <- b1.authors
  a2 <- b2.authors
  if a1 == a2
} yield a1

for (b <- books; a <- b.authors if a startsWith "C")
  yield b.title

books flatMap(b =>
  for(a <- b.authors if a startsWith "C") yield b.title)

books flatMap(b =>
  for(a <- b.authors.withFilter(a => a startsWith "C")) yield b.title)

books flatMap(b =>
  b.authors withFilter(a => a startsWith "C") map (a => b.title))
