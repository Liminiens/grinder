using System.ComponentModel.DataAnnotations;

namespace Grinder.DataAccess
{
  public class Message
    {
        [Required]
        public long MessageId { get; set; }

        [Required]
        public long ChatId { get; set; }
    
        [Required]
        public long UserId { get; set; }
    }
}