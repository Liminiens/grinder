using System.IO;
using Microsoft.EntityFrameworkCore;

namespace Grinder.DataAccess
{
  public class GrinderContext : DbContext
  {
    public DbSet<User> Users { get; set; }
    public DbSet<Message> Messages { get; set; }
    public DbSet<AdminUser> AdminUsers { get; set; }
    public DbSet<ChatToMonitor> ChatsToMonitor { get; set; }

    public static void MigrateUp()
    {
      Directory.CreateDirectory(Path.Combine(Directory.GetCurrentDirectory(), "data"));
      using (var context = new GrinderContext())
      {
        context.Database.Migrate();
      }
    }

    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
      modelBuilder.Entity<User>()
          .HasIndex(b => new { b.Username, b.UserId })
          .IsUnique();

      modelBuilder.Entity<Message>()
          .HasIndex(b => new { b.ChatId, b.UserId })
          .IsUnique();

      modelBuilder.Entity<Message>()
          .HasIndex(b => b.Date);

      modelBuilder.Entity<AdminUser>()
          .HasIndex(b => b.Username);

      modelBuilder.Entity<ChatToMonitor>()
          .HasIndex(b => b.Username);
    }

    protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
    {
      optionsBuilder.UseSqlite("Data Source=./data/grinder.db");
    }
  }
}