// package ru.tinkoff.phobos.decoding.instances.element

// import ru.tinkoff.phobos.decoding._
// import com.fasterxml.aalto.AsyncXMLStreamReader
// import scala.annotation.tailrec

// class GenericDecoder[T](state: DecoderState) extends ElementDecoder[T]:
//   override def decodeAsElement(
//     cursor : Cursor,
//     localName: String,
//     namespaceUri: Option[String],
//   ) = {
//     @tailrec
//     def go(currentState: DecoderState): ElementDecoder[T] = 
//       if cursor.getEventType() == AsyncXMLStreamReader.EVENT_INCOMPLETE then
//         cursor.next()
//         ??? // new $decoderName(currentState, ..${allParams.map(_.decoderConstructionParam)})
//       else currentState match 
//         case DecoderState.New =>
//           if cursor.isStartElement() then
//             errorIfWrongName[T](cursor, localName, namespaceUri) match 
//               case Some(error) => error
//               case None =>
//                 // ..$decodeAttributes
//                 cursor.next()
//                 go(DecoderState.DecodingSelf)
//           else 
//             new FailedDecoder[T](cursor.error("Illegal state: not START_ELEMENT"))

//         case DecoderState.DecodingSelf =>
//           // $parseTextParam
//           if cursor.isStartElement() then
//             cursor.getLocalName() match 
//               // case ..${decodeElements :+
//               //   cq"""field =>
//               //     cursor.next()
//               //     go($decoderStateObj.IgnoringElement(field, 0))
//               //   """}
//               case _ => ???
//           else if cursor.isEndElement() then
//             cursor.getLocalName() match 
//               case field if field == localName =>
//                 null /*$classConstruction*/ match
//                   case Right(result) =>
//                     cursor.next()
//                     new ConstDecoder[T](result)

//                   case Left(error) => new FailedDecoder[T](error)

//               case _ =>
//                 cursor.next()
//                 go(DecoderState.DecodingSelf)
//           else 
//             cursor.next()
//             go(DecoderState.DecodingSelf)

//         case DecoderState.DecodingElement(field) =>
//           field match 
//             // case ..$decodeElements
//             case _ => ???
//         case DecoderState.IgnoringElement(field, depth) =>
//           if cursor.isEndElement() && cursor.getLocalName() == field then
//             cursor.next()
//             if depth == 0 then
//               go(DecoderState.DecodingSelf)
//             else 
//               go(DecoderState.IgnoringElement(field, depth - 1))
//           else if cursor.isStartElement() && cursor.getLocalName() == field then
//             cursor.next()
//             go(DecoderState.IgnoringElement(field, depth + 1))
//           else 
//             cursor.next()
//             go(currentState)
//     end go

//     go(state)
//   }

//   def result(history: List[String]): Either[DecodingError, T] =
//     Left(DecodingError("Decoding not complete", history))

//   val isCompleted: Boolean = false

// end GenericDecoder

// enum DecoderState with
//   case New
//   case DecodingSelf
//   case DecodingElement(field: String)
//   case IgnoringElement(field: String, depth: Int)
// end DecoderState