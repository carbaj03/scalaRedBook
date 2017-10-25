object chapter6State {

  case class State[S, +A](run: S => (A, S)) {

    def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
  }

}
