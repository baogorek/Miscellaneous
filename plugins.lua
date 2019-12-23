local iron = require('iron')

iron.core.add_repl_definitions {
  python = {
    custom_python = {
      command = "ipython --simple-prompt"
    }
  },
  clojure = {
    lein_connect = {
      command = {"lein", "repl", ":connect"}
    }
  }
}

iron.core.set_config {
  preferred = {
    python = "custom_python",
    R = "R"
  }
}
