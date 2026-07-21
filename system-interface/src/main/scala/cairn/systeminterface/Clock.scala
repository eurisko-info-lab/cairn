package cairn.systeminterface

/** Effect family: wall-clock time (Phase 3). */
object Clock:
  enum Request:
    case Now
    case TimestampSlug  // filesystem-safe local timestamp

  enum Response:
    case Instant(epochMillis: Long)
    case Slug(value: String)

  enum Error:
    case Unavailable(message: String)
