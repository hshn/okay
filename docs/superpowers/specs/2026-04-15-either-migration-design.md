# Validation結果のEither化

## 概要

`Validation[-R, +V, -A, +B]` から ZIO 依存を除去し、`Validation[+V, -A, +B]` に変更する。
`run` の戻り値を `ZIO[R, Violations[V], B]` から `Either[Violations[V], B]` に変える。

## 動機

- `R`（ZIO環境）パラメータは全ての組み込みバリデータで `Any` であり、実質未使用
- DI が必要なバリデーション（DB重複チェック、時刻依存）は Aggregate やアプリケーション層の責務
- バリデーションライブラリの本質は純粋な入力変換であり、`Either` が責務を正確に表現する
- ZIO を知らないユーザーでも利用可能になる

## スコープ

- **対象**: core モジュールのみ
- **対象外**: zio-prelude モジュール（後続で対応。core の型変更によりコンパイル不可になる）

## 変更内容

### Validation 型

```scala
// Before
sealed abstract class Validation[-R, +V, -A, +B]:
  def run(a: A): ZIO[R, Violations[V], B]

// After
sealed abstract class Validation[+V, -A, +B]:
  def run(a: A): Either[Violations[V], B]
```

### ファクトリメソッド

全メソッドから `R` パラメータを除去。ZIO API を Either / Try / パターンマッチに置換。

| ZIO API | Either 対応 |
|---------|------------|
| `ZIO.succeed(x)` | `Right(x)` |
| `ZIO.fail(v)` | `Left(v)` |
| `ZIO.fromEither(e)` | `e`（そのまま） |
| `ZIO.attempt(x).refineOrDie(...)` | `try ... catch ...` |
| `for b <- run(a) yield ...` | `for b <- run(a) yield ...`（Either の flatMap） |
| `.catchAll { e => ... }` | パターンマッチ |
| `.mapError(f)` | `.left.map(f)` |

### ValidateN（エラー蓄積）

`zipPar` による並列実行を純粋な Either タプル合成に置換。

```scala
sealed trait ValidateTuple[V, T <: Tuple, Out <: Tuple]:
  def validate(t: T): Either[Violations[V], Out]
```

`R` パラメータ消滅。`ZIO.succeed(Right(...))` の二重ラップが不要に。

### ValidateAs / ValidatedAs

```scala
// Before
sealed trait ValidatedAs[-A, +B]:
  type Env
  type Err
  def run(a: A): ZIO[Env, Violations[Err], B]

// After
sealed trait ValidatedAs[-A, +B]:
  type Err
  def run(a: A): Either[Violations[Err], B]
```

`type Env` を除去。`.at()` 拡張は `Either[Violations[V], A]` 上の `.left.map` で実装。

### Cursor

`CursorValidateAs` と `CursorField` の戻り値型を `Either[Violations[va.Err], B]` に変更。
`.mapError` → `.left.map`。ロジック自体の変更なし。

### ValidationInstances（コレクション系）

`ZIO.foreach(...).map(...).absolve` → `map` + `partitionMap` に簡素化。

### build.sbt

```scala
lazy val core = (project in file("core"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-test"          % zio % Test,
      "dev.zio" %% "zio-test-sbt"      % zio % Test,
      "dev.zio" %% "zio-test-magnolia" % zio % Test,
    ),
  )
```

compile スコープの `"dev.zio" %% "zio"` を除去。zio-test が推移的に含むためtest用の明示追加も不要。

### テスト

`ZIOSpecDefault` と `test()` をそのまま使用。`test()` は `Either` を直接受け付けるためラップ不要。

### ドキュメント（docs/mdoc/index.md）

`Unsafe.unsafe { Runtime.default.unsafe.run(...) }` → `Either` のパターンマッチに書き換え。

## 変更対象ファイル一覧

### core main（5ファイル）
- `core/src/main/scala/yoshi/Validation.scala`
- `core/src/main/scala/yoshi/Cursor.scala`
- `core/src/main/scala/yoshi/syntax/ValidateN.scala`
- `core/src/main/scala/yoshi/syntax/ValidateAs.scala`
- `core/src/main/scala/yoshi/ValidationInstances.scala`

### defaults（2ファイル — 型引数の機械的変更のみ）
- `core/src/main/scala/yoshi/defaults/ValidationInstances.scala`
- `core/src/main/scala/yoshi/defaults/Validations.scala`

### build / docs
- `build.sbt`
- `docs/mdoc/index.md`

### テスト（7ファイル）
- `core/src/test/scala/yoshi/CursorSpec.scala`
- `core/src/test/scala/yoshi/ValidationsSpec.scala`
- `core/src/test/scala/yoshi/ValidationProductSpec.scala`
- `core/src/test/scala/yoshi/ValidationCombinatorsSpec.scala`
- `core/src/test/scala/yoshi/ValidationTransformsSpec.scala`
- `core/src/test/scala/yoshi/ValidationTypeclassSpec.scala`
- `core/src/test/scala/yoshi/ViolationsSpec.scala`

### 変更なし
- `core/src/main/scala/yoshi/Violations.scala`
- `core/src/main/scala/yoshi/internal/FieldNameMacro.scala`
- `core/src/main/scala/yoshi/defaults/Violation.scala`

### スコープ外（コンパイル不可になるが後続対応）
- `zio-prelude/` 配下の全ファイル
