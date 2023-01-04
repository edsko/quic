use std::{
    pin::Pin,
    task::{Context, Poll},
};

use bytes::Bytes;
use futures::{Future, Stream};
use quinn::{
    Accept, AcceptBi, AcceptUni, Connecting, Connection, Endpoint, ReadDatagram, RecvStream,
    SendStream,
};

/*******************************************************************************
  Stream connections
*******************************************************************************/

pub struct EndpointStream<'a> {
    endpoint: &'a Endpoint,
    accept: Option<Pin<Box<Accept<'a>>>>,
}

impl<'a> EndpointStream<'a> {
    pub fn new(endpoint: &'a Endpoint) -> Self {
        EndpointStream {
            endpoint,
            accept: None,
        }
    }
}

impl<'a> Stream for EndpointStream<'a> {
    type Item = Connecting;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        match &mut self.accept {
            None => {
                let mut accept = Box::pin(self.endpoint.accept());
                match accept.as_mut().poll(cx) {
                    Poll::Ready(c) => {
                        // We don't really expect this case to happen: when we
                        // accept a new connection, it won't come in immediately
                        Poll::Ready(c)
                    }
                    Poll::Pending => {
                        self.accept = Some(accept);
                        Poll::Pending
                    }
                }
            }
            Some(accept) => match accept.as_mut().poll(cx) {
                Poll::Ready(c) => {
                    self.accept = None;
                    Poll::Ready(c)
                }
                Poll::Pending => {
                    // We don't really expect this case to happen: when we are
                    // woken up, the connection should be ready
                    Poll::Pending
                }
            },
        }
    }
}

/*******************************************************************************
  Stream bidirectional streams on a given connection
*******************************************************************************/

pub struct BiStream<'a> {
    connection: &'a Connection,
    accept: Option<Pin<Box<AcceptBi<'a>>>>,
}

impl<'a> BiStream<'a> {
    pub fn new(connection: &'a Connection) -> Self {
        BiStream {
            connection,
            accept: None,
        }
    }
}

impl<'a> Stream for BiStream<'a> {
    type Item = (SendStream, RecvStream);

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        match &mut self.accept {
            None => {
                let mut accept = Box::pin(self.connection.accept_bi());
                match accept.as_mut().poll(cx) {
                    Poll::Pending => {
                        self.accept = Some(accept);
                        Poll::Pending
                    }
                    Poll::Ready(Ok(stream)) => Poll::Ready(Some(stream)),
                    Poll::Ready(Err(_)) => Poll::Ready(None),
                }
            }
            Some(accept) => match accept.as_mut().poll(cx) {
                Poll::Ready(Ok(stream)) => {
                    self.accept = None;
                    Poll::Ready(Some(stream))
                }
                Poll::Ready(Err(_)) => {
                    self.accept = None;
                    Poll::Ready(None)
                }
                Poll::Pending => Poll::Pending,
            },
        }
    }
}

/*******************************************************************************
  Stream unidirectional streams on a given connection
*******************************************************************************/

pub struct UniStream<'a> {
    connection: &'a Connection,
    accept: Option<Pin<Box<AcceptUni<'a>>>>,
}

impl<'a> UniStream<'a> {
    pub fn new(connection: &'a Connection) -> Self {
        UniStream {
            connection,
            accept: None,
        }
    }
}

impl<'a> Stream for UniStream<'a> {
    type Item = RecvStream;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        match &mut self.accept {
            None => {
                let mut accept = Box::pin(self.connection.accept_uni());
                match accept.as_mut().poll(cx) {
                    Poll::Pending => {
                        self.accept = Some(accept);
                        Poll::Pending
                    }
                    Poll::Ready(Ok(recv)) => Poll::Ready(Some(recv)),
                    Poll::Ready(Err(_)) => Poll::Ready(None),
                }
            }
            Some(accept) => match accept.as_mut().poll(cx) {
                Poll::Ready(Ok(recv)) => {
                    self.accept = None;
                    Poll::Ready(Some(recv))
                }
                Poll::Ready(Err(_)) => {
                    self.accept = None;
                    Poll::Ready(None)
                }
                Poll::Pending => Poll::Pending,
            },
        }
    }
}

/*******************************************************************************
  Stream datagrams on a given connection
*******************************************************************************/

pub struct DatagramStream<'a> {
    connection: &'a Connection,
    accept: Option<Pin<Box<ReadDatagram<'a>>>>,
}

impl<'a> DatagramStream<'a> {
    pub fn new(connection: &'a Connection) -> Self {
        DatagramStream {
            connection,
            accept: None,
        }
    }
}

impl<'a> Stream for DatagramStream<'a> {
    type Item = Bytes;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        match &mut self.accept {
            None => {
                let mut accept = Box::pin(self.connection.read_datagram());
                match accept.as_mut().poll(cx) {
                    Poll::Pending => {
                        self.accept = Some(accept);
                        Poll::Pending
                    }
                    Poll::Ready(Ok(bytes)) => Poll::Ready(Some(bytes)),
                    Poll::Ready(Err(_)) => Poll::Ready(None),
                }
            }
            Some(accept) => match accept.as_mut().poll(cx) {
                Poll::Ready(Ok(bytes)) => {
                    self.accept = None;
                    Poll::Ready(Some(bytes))
                }
                Poll::Ready(Err(_)) => {
                    self.accept = None;
                    Poll::Ready(None)
                }
                Poll::Pending => Poll::Pending,
            },
        }
    }
}
