\name{datamodel}
\alias{datamodel}
\title{Summarize data model of an editarray in a data.frame}
\usage{
  datamodel(E)
}
\arguments{
  \item{E}{\code{\link{editarray}}}
}
\value{
  \code{data.frame} describing the categorical variables
  and their levels.
}
\description{
  Summarize data model of an editarray in a data.frame
}
\examples{

E <- editarray(expression(
    age \%in\% c('under aged','adult'),
    positionInHouseholda  \%in\% c('marriage partner', 'child', 'other'),
    maritalStatus \%in\% c('unmarried','married','widowed','divorced'),
    if (maritalStatus \%in\% c('married','widowed','divorced') ) positionInHousehold != 'child',
    if ( age == 'under aged') maritalStatus == 'unmarried'
    )
)
datamodel(E)
}
\seealso{
  \code{\link{checkDatamodel}}
}

