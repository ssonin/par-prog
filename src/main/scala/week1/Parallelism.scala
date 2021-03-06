package week1

import java.util.concurrent.ThreadLocalRandom

object Parallelism extends App {

  def startThread(a: Account, b: Account, n: Int): Thread = {
    val t = new Thread {
      override def run(): Unit = {
        for (_ <- 0 until n) a.transfer(b, 1)
      }
    }
    t.start()
    t
  }

  val a1 = new Account(500000)
  val a2 = new Account(700000)
  val t1: Thread = startThread(a1, a2, 150000)
  val t2: Thread = startThread(a2, a1, 150000)
  t1.join()
  t2.join()
  println(a1.amount)
  println(a2.amount)
}

class Account(var amount: Int = 0) {

  private val uid: Long = getUniqueId

  def transfer(target: Account, n: Int): Unit = {
    if (this.uid < target.uid) lockAndTransfer(target, n)
    else target.lockAndTransfer(this, -n)
  }

  private def lockAndTransfer(target: Account, n: Int): Unit = {
    this.synchronized {
      target.synchronized {
        this.amount -= n
        target.amount += n
      }
    }
  }

  private def getUniqueId: Long = ThreadLocalRandom.current().nextLong()
}