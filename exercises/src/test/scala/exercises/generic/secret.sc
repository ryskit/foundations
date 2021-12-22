import exercises.generic.GenericFunctionExercises._

secret
secret.map(_.reverse)

secret.map(bytes => new String(bytes.toArray)).map(_.reverse).swap
