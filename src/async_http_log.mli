val src : Logs.Src.t

include Logs.LOG

module Res : sig
  val src : Logs.Src.t

  include Logs.LOG
end
