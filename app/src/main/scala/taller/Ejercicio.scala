package taller

// Un elemento de una lista anidada: un número o una sublista.
sealed trait Elemento
case class Valor(n: Int) extends Elemento
case class Sub(elementos: List[Elemento]) extends Elemento

// Una corrida es un valor y cuántas veces seguidas aparece.
case class Corrida(valor: Int, veces: Int)

class Ejercicio {

  // Punto 1. Recursión con if/else y las primitivas head, tail e isEmpty.
  // No use los métodos equivalentes de la biblioteca: el ejercicio es
  // escribirlos.

  /** Cuántos elementos tiene la lista. */
  def longitud(l: List[Int]): Int = {
    if (l.isEmpty) 0
    else 1 + longitud(l.tail)
  }

  /** La lista con los elementos en orden inverso. */
  def invertir(l: List[Int]): List[Int] = {
    if (l.isEmpty) List()
    else concatenar(invertir(l.tail), List(l.head))
  }

  /** Los elementos de l1 seguidos de los de l2. */
  def concatenar(l1: List[Int], l2: List[Int]): List[Int] = {
    if (l1.isEmpty) l2
    else l1.head :: concatenar(l1.tail, l2)
  }

  /** El último elemento. Falla con una lista vacía. */
  def ultimo(l: List[Int]): Int = {
    if (l.isEmpty) throw new NoSuchElementException("la lista está vacía")
    else if (l.tail.isEmpty) l.head
    else ultimo(l.tail)
  }

  /** Los primeros n elementos. Con n mayor que la longitud, la lista entera. */
  def tomar(l: List[Int], n: Int): List[Int] = {
    if (n <= 0 || l.isEmpty) List()
    else l.head :: tomar(l.tail, n - 1)
  }

  /** La lista sin sus primeros n elementos. */
  def soltar(l: List[Int], n: Int): List[Int] = {
    if (n <= 0 || l.isEmpty) l
    else soltar(l.tail, n - 1)
  }

  /** El elemento en la posición n, contando desde cero. */
  def enPosicion(l: List[Int], n: Int): Int = {
    if (n < 0 || n >= longitud(l)) throw new IndexOutOfBoundsException(n.toString)
    else if (n == 0) l.head
    else enPosicion(l.tail, n - 1)
  }

  // Punto 2. Recursión con match y los patrones Nil, x :: xs y sus variantes.

  /** La suma de los elementos; 0 para la lista vacía. */
  def sumar(l: List[Int]): Int = {
    l match {
      case Nil     => 0
      case x :: xs => x + sumar(xs)
    }
  }

  /** Si e aparece en la lista. */
  def contiene(l: List[Int], e: Int): Boolean = {
    l match {
      case Nil     => false
      case x :: xs => x == e || contiene(xs, e)
    }
  }

  /** La lista sin su último elemento. Falla con una lista vacía. */
  def inicio(l: List[Int]): List[Int] = {
    l match {
      case Nil      => throw new NoSuchElementException("la lista está vacía")
      case _ :: Nil => Nil
      case x :: xs  => x :: inicio(xs)
    }
  }

  /** Si cada elemento es estrictamente menor que el siguiente. */
  def esCreciente(l: List[Int]): Boolean = {
    l match {
      case x :: y :: _ => x < y && esCreciente(l.tail)
      case _           => true
    }
  }

  // Punto 3. Ordenamiento por inserción.

  /** Pone x en su lugar dentro de una lista que ya está ordenada. */
  def insertar(x: Int, l: List[Int]): List[Int] = {
    l match {
      case Nil           => List(x)
      case y :: ys if x <= y => x :: l
      case y :: ys       => y :: insertar(x, ys)
    }
  }

  /** La lista ordenada de menor a mayor. */
  def ordenar(l: List[Int]): List[Int] = {
    l match {
      case Nil     => Nil
      case x :: xs => insertar(x, ordenar(xs))
    }
  }

  // Punto 4. Aplanar una lista anidada.

  /** Los números de la lista, en una sola lista y en el mismo orden,
    * sin importar a qué profundidad estén. */
  def aplanar(l: List[Elemento]): List[Int] = {
    l match {
      case Nil                => Nil
      case Valor(n) :: resto  => n :: aplanar(resto)
      case Sub(es) :: resto   => aplanar(es) ++ aplanar(resto)
    }
  }

  // Punto 5. Codificación por corridas y su inversa.

  /** Las corridas de la lista: cada valor con cuántas veces seguidas
    * aparece, en el orden en que aparecen. */
  def corridas(l: List[Int]): List[Corrida] = {
    l match {
      case Nil => Nil
      case x :: _ =>
        val (iguales, otros) = l.span(_ == x)
        Corrida(x, iguales.length) :: corridas(otros)
    }
  }

  /** La lista que describen las corridas: cada valor repetido tantas veces
    * como dice su corrida. Una corrida con cero veces no aporta elementos. */
  def expandir(c: List[Corrida]): List[Int] = {
    c match {
      case Nil => Nil
      case Corrida(v, k) :: resto => List.fill(k)(v) ++ expandir(resto)
    }
  }
}
