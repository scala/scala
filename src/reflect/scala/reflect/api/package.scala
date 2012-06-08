package scala.reflect

package object api {

  // type and value aliases for slices of the base Universe cake that are not
  // repeated in api.Universe
  type Scopes = base.Scopes
  type BuildUtils = base.BuildUtils
  type Attachments = base.Attachments

  type MirrorOf[U <: base.Universe with Singleton] = base.MirrorOf[U]
}
